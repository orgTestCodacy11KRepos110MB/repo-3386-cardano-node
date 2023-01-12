usage_nixops() {
     usage "nixops" "Backend:  manages a cluster using 'nixops'" <<EOF

    Please see documentation for 'wb backend' for the supported commands.

    Nixops-specific:

    save-child-pids RUN-DIR
    save-pid-maps RUN-DIR
EOF
}

backend_nixops() {
op=${1:?$(usage_nixops)}; shift

case "$op" in
    name )
        echo 'nixops';;

    is-running )
        local usage="USAGE: wb nixops $op RUN-DIR"
        test -f "${1:?$usage}"/flag/is-running;;

    setenv-defaults )
        local usage="USAGE: wb nixops $op BACKEND-DIR"
        local backend_dir=${1:?$usage}

        setenvjqstr 'deployment' $(basename $(pwd))
        ;;

    allocate-run )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_nixops;;
               * ) break;; esac; shift; done

        mkdir -p           "$dir"/nixops

        local depl=$(envjqr 'deployment')
        local phy=aws
        # local phy=libvirtd

        local crmod=$(if nixops list | grep --quiet --no-messages -F "| $depl "
                      then echo modify
                      else echo create
                      fi)
        local crmod_args=(
            --deployment $depl
            -I profileJson="$dir"/profile.json

            nix/workbench/backend/nixops/physical-$phy.nix
        )
        nixops $crmod "${crmod_args[@]}"

        local deploy_args=(
            --deployment $depl
            -I profileJson="$dir"/profile.json
            --allow-reboot
            --confirm
            --copy-only
            --cores 0
            --no-build-output
            -j 4
        )
        nixops deploy "${deploy_args[@]}"
        ;;

    describe-run )
        local usage="USAGE: wb nixops $op RUN-DIR"

        cat <<EOF
  - NixOps deployment:       $(envjq 'deployment')
EOF
        ;;

    start-node )
        fail "NixOps backend does not implement operation:  $1";;
    stop-node )
        fail "NixOps backend does not implement operation:  $1";;

    wait-node )
        local usage="USAGE: wb nixops $op RUN-DIR [NODE-NAME]"
        local dir=${1:?$usage}; shift
        local node=${1:-$(dirname $CARDANO_NODE_SOCKET_PATH | xargs basename)}; shift
        local socket=$(backend_nixops get-node-socket-path "$dir" $node)

        local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' $dir/profile.json) i=0
        echo -n "workbench:  nixops:  waiting ${patience}s for socket of $node: " >&2
        while test ! -S $socket
        do printf "%3d" $i; sleep 1
           i=$((i+1))
           if test $i -ge $patience
           then echo
                progress "nixops" "$(red FATAL):  workbench:  nixops:  patience ran out for $(white $node) after ${patience}s, socket $socket"
                backend_nixops stop-cluster "$dir"
                fatal "$node startup did not succeed:  check logs in $(dirname $socket)/stdout & stderr"
           fi
           echo -ne "\b\b\b"
        done >&2
        echo " $node up (${i}s)" >&2
        ;;

    start-nodes )
        local usage="USAGE: wb nixops $op RUN-DIR [HONOR_AUTOSTART=]"
        local dir=${1:?$usage}; shift
        local honor_autostart=${1:-}

        local nodes=($(jq_tolist keys "$dir"/node-specs.json))

        if test -n "$honor_autostart"
        then for node in ${nodes[*]}
             do jqtest ".\"$node\".autostart" "$dir"/node-specs.json &&
                     nixopsctl start $node; done;
        else nixopsctl start ${nodes[*]}; fi

        for node in ${nodes[*]}
        do jqtest ".\"$node\".autostart" "$dir"/node-specs.json &&
                backend_nixops wait-node "$dir" $node; done

        if test ! -v CARDANO_NODE_SOCKET_PATH
        then export  CARDANO_NODE_SOCKET_PATH=$(backend_nixops get-node-socket-path "$dir" 'node-0')
        fi

        backend_nixops save-child-pids "$dir"
        backend_nixops save-pid-maps   "$dir"
        ;;

    start )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        if ! nixopsd --config  "$dir"/nixops/nixopsd.conf $@
        then progress "nixops" "$(red fatal: failed to start) $(white nixopsd)"
             echo "$(red nixopsd.conf) --------------------------------" >&2
             cat "$dir"/nixops/nixopsd.conf
             echo "$(red nixopsd.log) ---------------------------------" >&2
             cat "$dir"/nixops/nixopsd.log
             echo "$(white -------------------------------------------------)" >&2
             fatal "could not start $(white nixopsd)"
        fi

        if jqtest ".node.tracer" "$dir"/profile.json
        then if ! nixopsctl start tracer
             then progress "nixops" "$(red fatal: failed to start) $(white cardano-tracer)"
                  echo "$(red tracer-config.json) ------------------------------" >&2
                  cat "$dir"/tracer/tracer-config.json
                  echo "$(red tracer stdout) -----------------------------------" >&2
                  cat "$dir"/tracer/stdout
                  echo "$(red tracer stderr) -----------------------------------" >&2
                  cat "$dir"/tracer/stderr
                  echo "$(white -------------------------------------------------)" >&2
                  fatal "could not start $(white cardano-tracer)"
             fi

             progress_ne "nixops" "waiting for $(yellow cardano-tracer) to create socket: "
             while test ! -e "$dir"/tracer/tracer.socket; do sleep 1; done
             echo $(green ' OK') >&2
             backend_nixops save-child-pids "$dir"
        fi;;

    get-node-socket-path )
        local usage="USAGE: wb nixops $op STATE-DIR NODE-NAME"
        local state_dir=${1:?$usage}
        local node_name=${2:?$usage}

        echo -n $state_dir/$node_name/node.socket
        ;;

    start-generator )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_nixops;;
               * ) break;; esac; shift; done

        if ! nixopsctl start generator
        then progress "nixops" "$(red fatal: failed to start) $(white generator)"
             echo "$(red generator.json) ------------------------------" >&2
             cat "$dir"/tracer/tracer-config.json
             echo "$(red tracer stdout) -----------------------------------" >&2
             cat "$dir"/tracer/stdout
             echo "$(red tracer stderr) -----------------------------------" >&2
             cat "$dir"/tracer/stderr
             echo "$(white -------------------------------------------------)" >&2
             fatal "could not start $(white nixopsd)"
        fi
        backend_nixops save-child-pids "$dir";;

    wait-node-stopped )
        local usage="USAGE: wb nixops $op RUN-DIR NODE"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        progress_ne "nixops" "waiting until $node stops:  ....."
        local i=0
        while nixopsctl status $node > /dev/null
        do echo -ne "\b\b\b\b\b"; printf "%5d" $i >&2; i=$((i+1)); sleep 1
        done >&2
        echo -e "\b\b\b\b\bdone, after $(with_color white $i) seconds" >&2
        ;;

    wait-pools-stopped )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local i=0 pools=$(jq .composition.n_pool_hosts $dir/profile.json) start_time=$(date +%s)
        msg_ne "nixops:  waiting until all pool nodes are stopped: 000000"
        touch $dir/flag/cluster-termination

        for ((pool_ix=0; pool_ix < $pools; pool_ix++))
        do while nixopsctl status node-${pool_ix} > /dev/null &&
                   test -f $dir/flag/cluster-termination
           do echo -ne "\b\b\b\b\b\b"; printf "%6d" $((i + 1)); i=$((i+1)); sleep 1; done
              echo -ne "\b\b\b\b\b\b"; echo -n "node-${pool_ix} 000000"
        done >&2
        echo -ne "\b\b\b\b\b\b"
        local elapsed=$(($(date +%s) - start_time))
        if test -f $dir/flag/cluster-termination
        then echo " All nodes exited -- after $(yellow $elapsed)s" >&2
        else echo " Termination requested -- after $(yellow $elapsed)s" >&2; fi
        ;;

    stop-cluster )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        nixopsctl stop all || true

        if test -f "${dir}/nixops/nixopsd.pid"
        then kill $(<${dir}/nixops/nixopsd.pid) $(<${dir}/nixops/child.pids) 2>/dev/null
        else pkill nixopsd
        fi
        ;;

    cleanup-cluster )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        msg "nixops:  resetting cluster state in:  $dir"
        rm -f $dir/*/std{out,err} $dir/node-*/*.socket $dir/*/logs/* 2>/dev/null || true
        rm -fr $dir/node-*/state-cluster/;;

    save-child-pids )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local svpid=$dir/nixops/nixopsd.pid
        local pstree=$dir/nixops/ps.tree
        pstree -p "$(cat "$svpid")" > "$pstree"

        local pidsfile="$dir"/nixops/child.pids
        { grep -e '---\|--=' "$pstree" || true; } |
          sed 's/^.*--[=-] \([0-9]*\) .*/\1/; s/^[ ]*[^ ]* \([0-9]+\) .*/\1/
              ' > "$pidsfile"
        ;;

    save-pid-maps )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local mapn2p=$dir/nixops/node2pid.map; echo '{}' > "$mapn2p"
        local mapp2n=$dir/nixops/pid2node.map; echo '{}' > "$mapp2n"
        local pstree=$dir/nixops/ps.tree

        for node in $(jq_tolist keys "$dir"/node-specs.json)
        do ## nixopsd's service PID is the immediately invoked binary,
           ## ..which isn't necessarily 'cardano-node', but could be 'time' or 'cabal' or..
           local service_pid=$(nixopsctl pid $node)
           if   test $service_pid = '0'
           then continue
           elif test -z "$(ps h --ppid $service_pid)" ## Any children?
           then local pid=$service_pid ## <-=^^^ none, in case we're running executables directly.
                ## ..otherwise, it's a chain of children, e.g.: time -> cabal -> cardano-node
           else local pid=$(grep -e "[=-] $(printf %05d $service_pid) " -A5 "$pstree" |
                            grep -e '---\|--=' |
                            head -n1 |
                            sed 's/^.*--[=-] \([0-9]*\) .*/\1/;
                                 s/^[ ]*[^ ]* \([0-9]*\) .*/\1/')
           fi
           if test -z "$pid"
           then warn "nixops" "failed to detect PID of $(white $node)"; fi
           jq_fmutate "$mapn2p" '. * { "'$node'": '$pid' }'
           jq_fmutate "$mapp2n" '. * { "'$pid'": "'$node'" }'
        done
        ;;

    * ) usage_nixops;; esac
}
