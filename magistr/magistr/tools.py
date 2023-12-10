from xml.dom.minidom import (parse, Element)
import json
import os
import glob
from tqdm import tqdm

class Sentence:
    def __init__(self, sent: Element):
        self.id = sent.getAttribute('ID')
        self.words = []
        
        self.parseWords(sent.getElementsByTagName('W'))
        
    def parseWords(self, rawWords):
        
        self.wordMap = {}
        
        for rawWord in rawWords:
            
            word = Word(rawWord, self.id)
            
            self.words.append(word)
            
            if word.dom == '_root':
                self.rootWord = word
                
            if word.dom in self.wordMap.keys():
                self.wordMap[word.dom].append(word)
            else:
                self.wordMap[word.dom] = [word]

        for word in self.words:
            if word.id in self.wordMap.keys():
                for d in self.wordMap[word.id]:
                    word.addWord(d)
        
        
class Word:
    def __init__(self, w: Element, sentId):
        self.dom = w.getAttribute('DOM')
        self.feat = w.getAttribute('FEAT')
        self.feat = self.feat.replace(' МЕТА', '').replace(' НАСТ ', ' ').replace(' СЛ', '').replace('COM', '').split()
        if self.feat == '':
            self.feat = ['S']
        self.id = w.getAttribute('ID')
        self.lemma = w.getAttribute('LEMMA')
        self.link = w.getAttribute('LINK')
        
        self.v = " ".join(t.nodeValue for t in w.childNodes if t.nodeType == t.TEXT_NODE).lower().strip()
        
        
        
        self.sentId = sentId
        
        self.connectedWords = []
        
    def addWord(self, w):
        self.connectedWords.append(w)
          

def parseSentences(xml_file):
    doc = parse(xml_file)
    sentences = doc.getElementsByTagName('S')
    res = []
    for sent in sentences:
        res.append(Sentence(sent))
    return res

from collections import deque

def find_word_depth(root, target_word):
    queue = deque([(root, 0)])
    
    while queue:
        current_word, depth = queue.popleft()
        
        if current_word.id == target_word.id:
            return depth
        
        for child in current_word.connectedWords:
            queue.append((child, depth+1))
    
    return None

def find_word_position(root, target_word):
    queue = deque([(root, 1)])

    current_tier = 1
    position_in_tier = 0
    
    tier_positions = {}

    while queue:
        current_node, tier = queue.popleft()
        
        if tier > current_tier:
            current_tier = tier
            position_in_tier = 0

        position_in_tier += 1
        
        tier_positions[current_node.id] = position_in_tier
        
        if current_node.id == target_word.id:
            return tier_positions[target_word.id]
        
        for child in current_node.connectedWords:
            queue.append((child, tier + 1))
    
    return None

ids = {}
i = 0

def generate_ids(sentences):
    global i
    global ids

    for sent in sentences:
        for w in sent.words:
            v = w.v
            if v not in ids:
                ids[v] = i
                i += 1

def generate_json(sentences):
    global ids
    res = []
    
    for sent in sentences:
        root = sent.wordMap['_root'][0]
        for w in sent.words:
            m = {}
            m["id"] = ids[w.v]
            m["word"] = w.v
            m["gramm"] = w.feat
            m["n_tree"] = int(sent.id)
            m["n_depth"] = find_word_depth(root, w)
            m["n_pos"] = find_word_position(root, w)
            m["connected_words"] = []
            for child in w.connectedWords:
                m_connected = {}
                f = True
                for mm in m["connected_words"]:
                    if mm["SSR"] == child.link:
                        mm["id"].append(ids[child.v])
                        f = False
                if f:
                    m_connected["SSR"] = child.link
                    m_connected["id"] = [ids[child.v]]
                    m["connected_words"].append(m_connected)
            
            res.append(m)
    return res 

def generate_addition_json(xml_file):
    sentences = parseSentences(xml_file)
    generate_ids(sentences)
    return generate_json(sentences)

#corpus_names = ['Tselina', 'sirija', 'pedagogika']
corpus_names = ['sirija']
for corpus in corpus_names:
    xml_file = f'2016/{corpus}.xml'
    d = generate_addition_json(xml_file)
    with open(f'magistr/{corpus}_word_info.json', 'w', encoding='utf-8') as f:
        json.dump(d, f, ensure_ascii = False, indent = 4)
    with open(f'magistr/{corpus}_word_ids.json', 'w', encoding='utf-8') as f:
        json.dump(ids, f, ensure_ascii = False, indent = 4)

