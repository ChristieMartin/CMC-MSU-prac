#ifndef TOKEN_TOKENIZATION_H
#define TOKEN_TOKENIZATION_H

#include <string>
#include <vector>
#include <set>
#include <map>


struct Borders { // Границы слова или предложения в изначальном тексте
    int begin;
    int end;
    Borders(int begin, int end): begin(begin), end(end) {}
};

class Sentence {
public:
    Sentence(std::wstring* text, int begin, int end): text(text), begin(begin), end(end) {
        sentenceParsing();
    }
    std::wstring getSentence() {
        return std::wstring(*text, begin, end - begin + 1);
    }
    int getNumberOfWords() {
        return words.size();
    }
    int getNumberOfInsertedSentences() {
        return insertedSentences.size();
    }
    std::wstring getWord(int i) {
        begin = words[i - 1].begin;
        end = words[i - 1].end;
        return std::wstring(*text, begin, end - begin + 1);
    }
    std::wstring getInsertedSentence(int i) {
        begin = insertedSentences[i - 1].begin;
        end = insertedSentences[i - 1].end;
        return std::wstring(*text, begin, end - begin + 1);
    }
    std::wstring outputFormat();


private:
    void sentenceParsing();

private:
    std::wstring* text;
    int begin;
    int end;
    std::vector<Borders> words;
    std::vector<Borders> insertedSentences;
};


class Tokenization {
public:
    Tokenization(const std::wstring& source, const std::string& pathToAbbreviationBase = "../base.txt");

    Sentence getSentence(int i) {
        return sentences[i];
    }
    int getNumberOfSentence() {
        return numberOfSentence;
    }
    std::wstring& getText() {
        return text;
    }

    double getPrecision() {
        return correct / getNumberOfSentence();
    }
    double getRecall() {
        return correct / original;
    }
    double getF1() {
        double P = correct / (correct + mistaken);
        double R = correct / (correct + skipped);
        return (2 * P * R) / (P + R);
    }

    std::map<std::wstring, int>& getAbbreviationDatabase() {
        return abbreviationDatabase;
    }


private:
    void textParsing();
    bool isEndOfSentence(int i, int numberOfWords);

    std::wstring previousPseudoWord(int i);
    std::wstring nextPseudoWord(int i);
    std::wstring nextRealWord(int i);
    std::wstring previousWordForAbbreviation(int i);

    bool isNumber(const std::wstring& str);

private:
    std::wstring text;
    int length;
    int numberOfSentence;
    std::vector<Sentence> sentences;

    int threshold;
    std::map<std::wstring, int> abbreviationDatabase; // База сокращений, заполняемая машинным обучением

    double correct;
    double mistaken;
    double skipped;
    double original;

    std::vector<int> mistakenEndsOfSentences;
    std::vector<int> skippedEndsOfSentences;

};


#endif
