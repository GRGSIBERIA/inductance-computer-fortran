import sys
import codecs
import re

DEBUG = True
XMATCH = re.compile(r" +X  +")      # X列とマッチする正規表現
SMATCH = re.compile(r"^ *\r*\n$")   # 空白行とマッチする正規表現
SPLIT_SPACE = re.compile(r" +")
HMATCH = re.compile(r" +.+ *")      # ヘッダ行とマッチする正規表現
DOUBLE_SPACE = re.compile(r"  +")

# レポートファイルのパス配列を作成する
def get_reportfile(confpath):
    reports = []
    with codecs.open(confpath, "r", encoding="utf-8", errors="replace") as f:
        for line in f:
            if "wirefile" in line:
                rec = [x.strip().replace("\"", "") for x in line.split(",")]
                reports.append({"part": rec[1], "path": rec[2]})
    return reports


# 節点数を数える関数
def count_tables(text):
    table_count = 0
    for _ in XMATCH.finditer(text):
        table_count += 1
    return table_count


# ステップ数を数える関数
def count_times(lines):
    times = []

    # 位置合わせをする
    num = 0
    while True:
        if XMATCH.match(lines[num]):
            num += 2
            break
        num += 1
    
    step_num = 0
    while num < len(lines):
        if len(lines[num]) < 2:
            break
        times.append(float(re.split(r" +", lines[num])[1]))
        step_num += 1
        num += 1
    
    return (step_num, times)


def generate_header(lines, num):
    # ヘッダ行を探す
    header = ""
    while True:
        num += 1
        while True:
            if "  X  " in lines[num]:
                header += lines[num].split("X")[1].strip()
                num += 1
                break
            header += lines[num].strip()
            num += 1
        break
    return (num + 1, header)    # +1 するのは空行を飛ばすため


# ヘッダーから節点番号を抽出する
def extract_nodenum_from_header(header):
    return int(header.split(":")[3].strip())


# ヘッダーから軸IDを抽出する
def extract_axis_from_header(header):
    return int(header.split(" ")[0].split(":")[1][1])


def get_data(num, lines, axisid, total_times, nodedata):
    axisid -= 1
    
    for i in range(total_times):
        nodedata[i][axisid] = float(re.split(r" +", lines[num])[2])
        num += 1
    return num + 2

def read_report(report):
    # すべての行を予め取得しておく
    text = ""
    with codecs.open(report["path"], "r", encoding="utf-8", errors="replace") as f:
        text = f.read()

    total_tables = count_tables(text)

    lines = text.split("\n")
    total_times, times = count_times(lines)
    data = {}       # data[ノード番号, 時間, 3次元]

    # このセクションでデータを作成しておいて，あとで一気に書き込むつもり
    count_table = 0
    num = 0
    while num < len(lines) and count_table < total_tables:
        num, header = generate_header(lines, num)
        nodeid = extract_nodenum_from_header(header)
        axisid = extract_axis_from_header(header)

        # キーが含まれていなければデータを追加する
        if nodeid not in data:
            data[nodeid] = [[0.0, 0.0, 0.0] for _ in range(total_times)]
        num = get_data(num, lines, axisid, total_times, data[nodeid])

        count_table += 1

    return times, data


def write_nwf_data(part, data, outfile):
    outfile.write("*Part,{},{}\n".format(part, len(data)))
    for nodeid, datum in data.items():
        outfile.write("{}\n".format(nodeid))

        for time4data in datum:
            outfile.write("{},{},{}\n".format(time4data[0], time4data[1], time4data[2]))


def write_nwf_times(times, outfile):
    outfile.write("*Times,{}\n".format(len(times)))
    for time in times:
        outfile.write("{}\n".format(time))

def write_nwf_info(total_part, outfile):
    outfile.write("{}\n".format(total_part))
    

def Main():
    confpath = "config.conf"
    outpath = "config.nwf"

    if not DEBUG:
        confpath = sys.argv[1]
        outpath = sys.argv[2]

    # path
    # part
    # data
    # times
    reports = get_reportfile(confpath)

    for report in reports:
        report["times"], report["data"] = read_report(report)

    with codecs.open(outpath, "w", encoding="utf-8") as outfile:
        if reports:
            write_nwf_info(len(reports), outfile)
            write_nwf_times(reports[0]["times"], outfile)

            for report in reports:
                write_nwf_data(report["part"], report["data"], outfile)


if __name__ == "__main__":
    Main()
