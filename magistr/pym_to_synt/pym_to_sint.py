import pymorphy2

morph = pymorphy2.MorphAnalyzer()

dict_file = 'pym_to_synt/sirija_dict.txt'
out_file = 'pym_to_synt/out.txt'
keys_out_file = 'pym_to_synt/keys_out.txt'
all_out_file = 'pym_to_synt/all_out.txt'

pts_m = {
    "nomn": ["ИМ", "СЛ", "COM"],
    "2per": ["2-Л"],
    "PRTF": ["ПРИЧ", "КР"],
    "NOUN": ["S"],
    "gen2": ["РОД"],
    "accs": ["ВИН"],
    "NUMB": ["NUM"],
    "past": ["ПРОШ", "КР"],
    "loc2": ["ПР"],
    #"intg": ["NUM"],
    "ADJS": ["A", "КР", "СЛ"],
    "loct": ["ПР"],
    "COMP": ["ADV", "СРАВ"],
    "inan": ["НЕОД"],
    "pres": ["НЕПРОШ"],
    "indc": ["ИЗЪЯВ"],
    "3per": ["3-Л"],
    "ADVB": ["ADV"],
    "ablt": ["ТВОР"],
    "sing": ["ЕД"],
    "Supr": ["ПРЕВ"],
    "intr": ["СТРАД"],
    "Cmp2": ["СМЯГ"],
    "neut": ["СРЕД"],
    "CONJ": ["CONJ"],
    "impf": ["НЕСОВ"],
    "anim": ["ОД", "A"],
    "GRND": ["ДЕЕПР", "V"],
    "NUMR": ["NUM"],
    "INFN": ["ИНФ", "V"],
    "femn": ["ЖЕН"],
    "gent": ["РОД"],
    "masc": ["МУЖ"],
    "ADJF": ["A"],
    "VERB": ["V"],
    "perf": ["СОВ"],
    "pssv": ["СТРАД"],
    "PRCL": ["PART", "V"],
    "PRTS": ["ПРИЧ", "КР", "V", "A"],
    "plur": ["МН"],
    "PREP": ["PR"],
    "datv": ["ДАТ"],
    "1per": ["1-Л"],
    #"LATN": ["S"],
    #"tran": ["V", "2-Л"],
    "impr": ["ПОВ"],
    "UNKN": ["NID"],
    "Name": ["COM", "СЛ"],
    "Fixd": ["COM", "СЛ"],
    "actv": ["V"],
    "Anph": ["A", "S", "МУЖ"],
    #"Apro": ["A", "S"],
    #"Subx": ["A", "S"],
    "Anum": ["NUM"],
    "Qual": ["A", "ADV"],
    #"NPRO": ["ОД", "НЕОД", "S"],
    "futr": ["НЕПРОШ"],
    "ROMN": ["NUM"],
    #"voct": ["СРЕД"],
    #"Abbr": ["NID"],
    #"INTJ": ["НАСТ", "PART", "CONJ", "PR", "КР"],
    "PRED": ["ADV"],
}

s_tags = list(set([item for sublist in pts_m.values() for item in sublist]))


all_tags = set()

m = {}

with open(dict_file, 'r', encoding='utf-8') as f:
    for line in f:
        gram = line.split('=')[0].strip()
        wrds = [i.strip() for i in line.split('=')[1].split('|')]
        tags = set()
        for word in wrds:
            l = morph.parse(word)
            for tag in l:
                t = set(tag.tag.grammemes)
                tags.update(t)
                all_tags.update(t)
        m[gram] = tags

with open(keys_out_file, 'w', encoding='utf-8') as f:
    al = set()
    for key in m:
        k = key.replace('[', '').replace(']', '')
        al.update(k.split(' '))
    for item in [i for i in al if i not in s_tags]:
        print(item, file=f)

with open(out_file, 'w', encoding='utf-8') as f:
    for key, value in m.items():
        print(str(key) + ':' + str(value), file=f)
    print('\n', file=f)
    
#with open(all_out_file, 'w', encoding='utf-8') as f:
#    for tag in all_tags:
#        print(tag, file=f)