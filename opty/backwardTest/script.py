import re
import subprocess
import os
import pandas as pd

def convert_to_list(value):
    if isinstance(value, range):
        # If the input is a range, convert it to a list
        return list(value)
    elif isinstance(value, int):
        # If the input is an integer, create a list with a single element
        return [value]
    else:
        # Handle other cases or raise an error if needed
        raise ValueError("Unsupported type")


def parse(text):
    # Define regular expressions to extract total transactions and percentages
    total_transactions_pattern = re.compile(r"Transactions TOTAL:(\d+)")
    percentage_pattern = re.compile(r"-> (\d+\.\d+) %")

    # Find all matches in the log messages
    total_transactions_matches = total_transactions_pattern.findall(text)
    percentage_matches = percentage_pattern.findall(text)
    return (total_transactions_matches, percentage_matches)


def measure_experiment(
    iterations, clients, entries, read, write, duration, subset, experiment
):
    subprocess.run(["erl", "-make"], check=False)
    output_dir = f"experiments/{experiment}"
    timeout = duration + 2
    for ex in experiment:
        if ex == "clients":
            clients = range(1, clients)
        if ex == "entries":
            entries = range(1, entries)
        if ex == "read":
            read = range(1, read)
        if ex == "write":
            write = range(1, write)
        if ex == "duration":
            duration = range(1, duration)
        if ex == "subset_size":
            subset = range(1, duration)

    dict_e = {
        "clients": convert_to_list(clients),
        "entries": convert_to_list(entries),
        "read": convert_to_list(read),
        "write": convert_to_list(write),
        "duration": convert_to_list(duration),
        "subset": convert_to_list(subset),
    }
    os.makedirs(output_dir, exist_ok=True)
    with open(f"{output_dir}/summary.csv", "w", encoding="utf-8") as summary_file:
        summary_file.write(
            "clients,entries,rxt,wxt,duration,subset_size,transactions,ok\n"
        )
        print(dict_e)
        for s in dict_e["subset"]:
            for c in dict_e["clients"]:
                for num_entries in dict_e["entries"]:
                    for rxt in dict_e["read"]:
                        for wxt in dict_e["write"]:
                            for duration in dict_e["duration"]:
                                envs = os.environ.copy()
                                envs["clients"] = str(c)
                                envs["entries"] = str(num_entries)
                                envs["read"] = str(rxt)
                                envs["write"] = str(wxt)
                                envs["duration"] = str(duration)
                                envs["subset"] = str(s)
                                if experiment == ["read", "write"] and rxt + wxt != 10:
                                    break
                                for it in range(iterations):
                                    print(
                                        f"Interation {it}: {envs['clients']} clients {envs['entries']} entries {envs['read']} rxt {envs['write']} wxt {envs['duration']} duration {envs['subset']} subset"
                                    )
                                    filename = f"{envs['clients']}clients_{envs['entries']}entries_{envs['read']}rxt_{envs['write']}wxt_{envs['duration']}duration_{envs['subset']}subset.{it}.out"
                                    with open(
                                        f"{output_dir}/{filename}",
                                        "a+",
                                        encoding="utf-8",
                                    ) as outfile:
                                        try:
                                            subprocess.run(
                                                [
                                                    "erl",
                                                    "-noshell",
                                                    "-pa",
                                                    "ebin",
                                                    "-eval",
                                                    f"opty:start({envs['clients']},{envs['entries']},{envs['read']},{envs['write']},{envs['duration']},{envs['subset']})",
                                                ],
                                                check=False,
                                                env=envs,
                                                timeout=timeout,
                                                stdout=outfile,
                                            )
                                        except subprocess.TimeoutExpired:
                                            outfile.seek(0)
                                            (trs, oks) = parse(outfile.read())
                                            for tr, ok in zip(trs, oks):
                                                summary_file.write(
                                                    f"{c},{num_entries},{rxt},{wxt},{duration},{s},{tr},{ok}\n"
                                                )
                                        summary_file.flush()


def print_avg(filepath, columns):
    df = pd.read_csv(filepath, sep=",")
    print(df.columns)
    transactions = df.groupby(columns)[["transactions"]].mean()
    trs_res = list(transactions.itertuples(index=True, name=None))
    oks = df.groupby(columns)[["ok"]].mean()
    oks_res = list(oks.itertuples(index=True, name=None))
    print("##### TRANSACTIONS #####")
    for tr in trs_res:
        print(f"({tr[0]}, {tr[1]})")
    print()
    print("##### Ok (%) #####")
    for ok in oks_res:
        print(f"({ok[0]}, {ok[1]})")

def print_stddev(filepath, columns):
    df = pd.read_csv(filepath, sep=",")
    oks = df.groupby(columns)[["ok"]].std()
    print(oks)
    oks_res = list(oks.itertuples(index=True, name=None))
    print("##### Stddev Ok (%) #####")
    for ok in oks_res:
        print(f"({ok[0]}, {ok[1]})")


def main():
    experiment = ["subset5"]
    filepath = f"experiments/{experiment[0]}/summary.csv"

    # usage iterations, clients, entries, read, write, duration,subset, experiment
    #measure_experiment(2, 2, 5, 6, 4, 10, 1, experiment)
    #print_avg(filepath, experiment)
    print_stddev(filepath, ["subset_size"])


if __name__ == "__main__":
    main()
