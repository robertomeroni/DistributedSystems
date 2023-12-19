#!/bin/bash

measure_delays(){
    erl -make
    iterations=5
    acceptor_delays=(100 200 300 400 500 600 700 800 900)
    output_dir=experiments/delays
    mkdir -p $output_dir
    echo Delay,Rounds,Time,Status >> $output_dir/summary.csv
    for d in "${acceptor_delays[@]}"; do
        for it in $(seq 1 $iterations); do
            echo 2000 timeout $d delay $it iter;
            filename=delay_"$d"."$it".out;
            export delay=$d; erl -noshell -pa ebin -eval "paxy:start([100, 100, 100])" > $output_dir/$filename & pid=$!; sleep 15; kill $pid
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

measure_delays
