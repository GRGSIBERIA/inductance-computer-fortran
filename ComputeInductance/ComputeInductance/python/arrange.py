import sys
import codecs
import re
import numpy as np

DEBUG = True

def get_inpfile(confpath):
    inppath = ""
    elementmode = False

    # 設定ファイルを読み込んでinpfileのパスを控える
    with codecs.open(confpath, "r", encoding="utf-8", errors="replace") as f:
        for line in f:
            # inpfileの文字列が来たらパスを覚える
            if "inpfile" in line:
                inppath = line.split(",")[1].strip()

            if "elementmode" in line and "true" in line.split(",")[1]:
                elementmode = True
    return (inppath[1:-1], elementmode)

def get_lines(inppath):
    lines = None
    with codecs.open(inppath, "r", encoding="utf-8", errors="replace") as f:
        lines = f.readlines()
    return lines

# ノードとベクトルの組を出力
def generate_node_vector(line):
    rec = [x.strip() for x in line.split(",")]
    result = [int(rec[0])]
    result.append(np.array([float(x) for x in rec[1:]], dtype="float64"))
    return result

def generate_vector(line):
    rec = [x.strip() for x in line.split(",")]
    return np.array([float(r) for r in rec], dtype="float64")

def extract_equal(line, attribute):
    data = line.split(",")
    for datum in data:
        element = [d.strip() for d in datum.split("=")]
        if attribute in element[0]:
            return element

# partが来たら処理対象として登録する
def add_part_name(line_num, lines, result):
    part_name = extract_equal(lines[line_num], "name")[1]
    result[part_name] = {"nodes": [], "elements": []}
    return (line_num, part_name)

# instanceが来たら次の行で座標値が来る可能性があるので登録する
def add_instance(line_num, lines, result):
    part_name = extract_equal(lines[line_num], "part")[1]
    if not "*" in lines[line_num+1]:
        result[part_name]["position"] = generate_vector(lines[line_num+1])
    return (line_num, part_name)

# 要素の情報を入力する
def generate_element(line):
    rec = [int(x.strip()) for x in line.split(",")]
    result = [int(rec[0])]
    result.append([r for r in rec[1:5]])
    return result

# 節点の座標値を入力する
def add_node_positions(line_num, lines, prev_part, elementmode, result):
    # 座標値の入力
    while not "*" in lines[line_num]:
        line = lines[line_num]
        try:
            result[prev_part]["nodes"].append(generate_node_vector(line))
        except ValueError:
            pass
        line_num += 1

    # 節点だけじゃなくて要素番号も続いている
    if "*Element" in lines[line_num] and elementmode:
        line_num += 1
        while not "*" in lines[line_num]:
            try:
                result[prev_part]["elements"].append(generate_element(lines[line_num]))
            except ValueError:
                pass
            line_num += 1
    return line_num

# ファイルを正規化して出力する
def export_file(path, elementmode, result):
    position = np.array([0.0, 0.0, 0.0], dtype="float64")

    with codecs.open(path, "w", encoding="utf-8") as f:
        for part, data in result.items():
            # 先頭行の作成
            if not elementmode:
                f.writelines("*Part, {}, {}\n".format(part, len(data["nodes"])))
            else:
                f.writelines("*Part, {}, {}, {}\n".format(part, len(data["nodes"]), len(data["elements"])))
            
            if "position" in data:
                position = data["position"]
            
            for datum in data["nodes"]:
                index = datum[0]
                coord = datum[1] + position
                f.writelines(("{}, {}, {}, {}\n".format(index, coord[0], coord[1], coord[2])))

            if elementmode:
                for datum in data["elements"]:
                    index = datum[0]
                    tetrahedron = datum[1:][0]
                    f.writelines("{}, {}, {}, {}, {}\n".format(index, tetrahedron[0], tetrahedron[1], tetrahedron[2], tetrahedron[3]))

def Main():
    confpath = "config.conf"
    outpath = "config.nif"  # normalized input node

    if not DEBUG:
        regexp = r"\n\n\n\n"
        confpath = re.sub(regexp, "\n\n", sys.argv[1])
        outpath = re.sub(regexp, "\n\n", sys.argv[2])

    (inppath, elementmode) = get_inpfile(confpath)
    lines = get_lines(inppath)
    
    # 内容を読み込んでメモリ上に展開する
    result = {}
    line_num = 0
    prev_part = ""
    while line_num < len(lines):
        # Part -> Nodeと続いた場合は3次元の座標値が続く
        if "*Part" in lines[line_num]:
            line_num, prev_part = add_part_name(line_num, lines, result)

        elif "*Instance" in lines[line_num]:
            line_num, prev_part = add_instance(line_num, lines, result)

        elif "*Node" in lines[line_num] and not "Output" in lines[line_num]:
            line_num = add_node_positions(line_num + 1, lines, prev_part, elementmode, result)

        line_num += 1
    
    export_file(outpath, elementmode, result)


if __name__ == "__main__":
    Main()
