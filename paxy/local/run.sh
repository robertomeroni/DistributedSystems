#!/bin/bash

measure_drop(){
    erl -make
    iterations=5
    drop_values=(10 20 30 40 50 60 70 80 90)
    output_dir=experiments/drops
    echo Drop,Rounds,Time,Status >> $output_dir/summary.csv
    mkdir -p $output_dir
    for r in "${drop_values[@]}"; do
        for it in $(seq 1 $iterations); do
            echo $r drop $it iter;
            filename=drop_"$r"."$it".out;
            export send_mode="dropped"; export drop=$r; erl -noshell -pa ebin -eval "paxy:start([100, 100, 100])" > $output_dir/$filename & pid=$!; sleep 35; kill $pid
            time=$(grep "Total elapsed time" $output_dir/$filename | tr -dc '0-9')
            round=$(grep "LAST ROUND" $output_dir/$filename | grep "LAST ROUND" | awk '{print $NF}' | sort -n | tail -n 1)
            is_finished="NO CONSENSUS"
            count=$(grep -c "LAST ROUND" "$output_dir/$filename")
            if [ $count -ge 3 ]; then
                is_finished="FINISHED"
            fi
            echo $r,$round,$time,$is_finished  >> $output_dir/summary.csv
        done
    done
    unset drop
}

measure_delays(){
    erl -make
    iterations=5
    acceptor_delays=(4100 4200 4300 4400 4500 4600 4700 4800 4900 5000 5100 5200 5300 5400 5500 5600 5700 5800 5900 6000) 
    output_dir=experiments/delays
    mkdir -p $output_dir
    echo Delay,Rounds,Time,Status >> $output_dir/summary.csv
    for d in "${acceptor_delays[@]}"; do
        for it in $(seq 1 $iterations); do
            echo 2000 timeout $d delay $it iter;
            filename=delay_"$d"."$it".out;
            export send_mode="delayed"; export delay=$d; erl -noshell -pa ebin -eval "paxy:start([100, 100, 100])" > $output_dir/$filename & pid=$!; sleep 50; kill $pid
            time=$(grep "Total elapsed time" $output_dir/$filename | tr -dc '0-9')
            round=$(grep "LAST ROUND" $output_dir/$filename | grep "LAST ROUND" | awk '{print $NF}' | sort -n | tail -n 1)
            is_finished="NO CONSENSUS"
            count=$(grep -c "LAST ROUND" "$output_dir/$filename")
            if [ $count -ge 3 ]; then
                is_finished="FINISHED"
            fi
            echo $d,$round,$time,$is_finished >> $output_dir/summary.csv
        done
    done
    unset delay
}

measure_accept_vs_prop(){
    erl -make
    iterations=5
    proposers=(3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
    acceptors=(5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
    output_dir=experiments/accep_prop
    echo Proposers,Acceptors,Rounds,Time,Status >> $output_dir/summary.csv
    mkdir -p $output_dir
    for p in "${proposers[@]}"; do
        for a in "${acceptors[@]}"; do
            for it in $(seq 1 $iterations); do
                echo $p proposers $a acceptors $it iter
                filename=prop_"$p"_accept_"$a"."$it".out
                export proposer=$p; export acceptor=$a; erl -noshell -pa ebin -eval "paxy:measure()" > $output_dir/$filename & pid=$!; sleep 35; kill $pid
                time=$(grep "Total elapsed time" $output_dir/$filename | tr -dc '0-9')
                round=$(grep "LAST ROUND" $output_dir/$filename | grep "LAST ROUND" | awk '{print $NF}' | sort -n | tail -n 1)
                is_finished="NO CONSENSUS"
                count=$(grep -c "LAST ROUND" "$output_dir/$filename")
                if [ $count -ge $p ]; then
                    is_finished="FINISHED"
                fi
                echo $p,$a,$round,$time,$is_finished >> $output_dir/summary.csv
            done
        done
    done
    unset proposer
    unset acceptor
}

measure_delays
#measure_drop
#measure_accept_vs_prop
