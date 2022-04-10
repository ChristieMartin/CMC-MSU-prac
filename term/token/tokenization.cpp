#include "tokenization.h"

Tokenization::Tokenization(const char* source, int n)
    : text()
    , length(n)
    , numberOfSentence(0)
    , sentenceBorders(std::vector<SentenceBorders>())
{
    text = new char[length + 1];
    strcpy(text, source);
    SentenceTokenization();
}

void Tokenization::SentenceTokenization() {
    int lastBegin = 0;
    for (int i = 0; i < length + 1; ++i) {
        if (isEndOfSentence(i) && lastBegin != i) {
            numberOfSentence++;
            sentenceBorders.emplace_back(SentenceBorders(lastBegin, i));
            lastBegin = i + 1;
        }
    }
}

bool Tokenization::isEndOfSentence(int i) {
    return text[i] == '.' || text[i] == '\0';
}

std::string Tokenization::getSentence(int i) {
    int begin = sentenceBorders[i - 1].begin;
    int end = sentenceBorders[i - 1].end;
    std::string res(text, begin, end - begin + 1);
    return res;
}

int Tokenization::getNumberOfSentence() {
    return numberOfSentence;
}

Tokenization::~Tokenization() {
    delete [] text;
}