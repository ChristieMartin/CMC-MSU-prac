import pymorphy2
import json
from tqdm import tqdm
import ast

morph = pymorphy2.MorphAnalyzer()

with open('json_generator/all_dict.json', 'r', encoding='utf-8') as f:
    all_dict = json.load(f)

with open('pym_to_synt/pymorphy_to_syntagrus.json', 'r', encoding='utf-8') as f:
    pts_m = json.load(f)
    
def to_syntagrus(word):
    m = morph.parse(word)

    l = []
    for p in m:
        t = set(p.tag.grammemes)
        l.append(t)
        
    res = []
    for item in l:
        res_item = []
        for tag in item:
            if tag in pts_m:
                res_item += pts_m[tag]
        res.append(set(res_item))
    return res, l

def find_err_map():

    # word: [
    #  {
    #   "synt": граммемы синтагруса
    #   "pym_to_synt": граммемы отображения
    #   "pym": граммемы pymorphy

    err_map = {}
    for grammems, words in tqdm(all_dict.items()):
        g = set(grammems.split(" "))
        for word in words:
            synt, pym_tags = to_syntagrus(word)
            b = True
            for v in synt:
                if v <= g or g <= v:
                    b = False
            if b:
            #if g not in synt:
                cur_m = {
                    "synt": g,
                    "pym_to_synt": synt,
                    "pym": pym_tags,
                }
                if word in err_map:
                    err_map[word].append(cur_m)
                else:
                    err_map[word] = [cur_m]
    return err_map

def print_err_map(err_map):
    with open("pym_to_synt/err_map_subset.txt", "w", encoding="utf-8") as f:
        for key, value in err_map.items():
            print(key, file=f)
            for val in value:
                for k, v in val.items():
                    print("\t" + str(k) + ":", file=f)
                    for vv in v:
                        print("\t\t" + str(vv), file=f)
            print('', file=f)
     
def print_err_map_keys(err_map):     
    with open("pym_to_synt/err_map_keys_subset.txt", "w", encoding="utf-8") as f:
        for key in err_map.keys():
            print(key, file=f)

def check_difference():
    all_words = []
    with open('json_generator/open_corpora_difference.txt', 'r', encoding='utf-8') as f:
        all_words = ast.literal_eval(f.read())
    not_found = set()
    similar = {}
    for word in tqdm(all_words):
        s, _ = to_syntagrus(word)
        b = False # если b внутри цикла стал True, то это слово, с подмножеством граммем
        for key, val in all_dict.items():
            gramms = key.split(' ')
            for vars in s:
                v = set(vars)
                g = set(gramms)
                #if v == g:
                #    b = True
                if v <= g:
                    b = True
                #if v <= g or g <= v:
                #    b = True
            '''
            b = b or check_word(s, set(gramms))
            if b:
                
                if word in similar:
                    similar[word].update(set(val))
                else:
                    similar[word] = set(val)
                break
            '''
            
        if not b: 
            not_found.add(word)   
        
    if len(not_found) != 0:
        print(f'not found similar words in syntagrus of {len(not_found)} open corpora words')

    #with open('pym_to_synt/open_corpora_similar.txt', 'w', encoding='utf-8') as f:
    #    for key, val in similar.items():
    #        key_gramms = to_syntagrus(key)
    #        print(f"{key} - {val}", file=f)

'''
all_vals = set()
for key, val in all_dict.items():       
    all_vals.update(set(val))
    
err_map = find_err_map()
print(len(all_vals))
print(len(err_map.keys()))

print_err_map(err_map)
print_err_map_keys(err_map)
'''

check_difference()
