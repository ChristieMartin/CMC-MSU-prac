#ifndef TOKEN_TOKENIZATION_H
#define TOKEN_TOKENIZATION_H

#include <string>
#include <vector>


struct Borders { // Границы слова или предложения в изначальном тексте
    int begin;
    int end;
    Borders(int begin, int end): begin(begin), end(end) {}
};

class Sentence {
public:
    Sentence(std::wstring* text, int begin, int end): text(text), begin(begin), end(end) {
        sentenceParsing();
    };
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
    Tokenization(const std::wstring& source);
    Sentence getSentence(int i);
    int getNumberOfSentence();

    std::wstring getText() {
        return std::wstring(text);
    }


private:
    void textParsing();
    bool isEndOfSentence(int i);

    std::wstring previousPseudoWord(int i);
    std::wstring nextPseudoWord(int i);
    std::wstring nextRealWord(int i);

private:
    std::wstring text;
    int length;
    int numberOfSentence;
    std::vector<Sentence> sentences;

    std::vector<std::wstring> abbreviationDatabase; // База сокращений, пока пустая, потом можно
                                                    // будет заполнить из файла

};


#endif
