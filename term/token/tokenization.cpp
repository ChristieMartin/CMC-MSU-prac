#include "tokenization.h"
#include <iostream>
#include <sstream>

Tokenization::Tokenization(const std::wstring& source)
    : text(source)
    , length(source.length())
    , numberOfSentence(0)
    , sentences(std::vector<Sentence>())
{
    textParsing();
}

void Tokenization::textParsing() { // Считываем текст и делим его на предложения
    int begin = 0;
    bool sentenceEnded = true;
    for (int i = 0; i < length; ++i) {
        if (sentenceEnded && (iswalpha(text[i]) || isdigit(text[i]))) {
            begin = i;
            sentenceEnded = false;
        }
        if (isEndOfSentence(i) && !sentenceEnded) {
            numberOfSentence++;
            sentences.emplace_back(Sentence(&text, begin, i));
            sentenceEnded = true;
        }
    }
}

bool Tokenization::isEndOfSentence(int i) { // Является ли точка окончанием предложения
    if (text[i] == '!' || text[i] == '?')
        return true;
    if (text[i] == '.') {
        std::wstring prev = previousPseudoWord(i); // Псевдослово до точки
                                                   // Псевдослово - набор символов без пробелов
        std::wstring next = nextPseudoWord(i);     // Псевдослово после точки
        std::wstring realNext = nextRealWord(i);   // "Настоящее" слово после точки
                                                   // Слово - псевдослово, включающее хотя бы одну букву или цифру

        if (prev[0] == '.') // проверка на троеточие
            return false;
        if (next == L"" || realNext == L"") // последнее предложение, нет следующего слова
            return true;
        if (!iswupper(realNext[0]) && !iswdigit(realNext[0])) // если следующее слово начинается не с большой буквы - не конец предложения
            return false;
        if (iswdigit(prev[0]) && iswdigit(text[i + 1])) // плавающая точка числа
            return true;
        for (int i = 0; i < abbreviationDatabase.size(); ++i) { // сравниваем слово до точки со словами из базы сокращений
            if (prev == abbreviationDatabase[i]) {
                return false;
            }
        }
      return true;
    }
    return false;
}

std::wstring Tokenization::previousPseudoWord(int i) {
    int begin = i - 1;
    while (begin >= 0 && !isspace(text[begin])) {
        begin--;
    }
    return std::wstring(text, begin + 1, i - begin - 1);
}

std::wstring Tokenization::nextPseudoWord(int i) {
    int begin = i + 1;
    while (begin < length && isspace(text[begin])) {
        begin++;
    }
    if (begin == length) {
        return L"";
    }
    int end = begin;
    while (end < length && !isspace(text[end])) {
        end++;
    }
    return std::wstring(text, begin, end - begin);
}

std::wstring Tokenization::nextRealWord(int i) {
    int begin = i + 1;
    while (begin < length && !iswalpha(text[begin]) && !iswdigit(text[begin])) {
        begin++;
    }
    if (begin == length) {
        return L"";
    }
    int end = begin;
    while (end < length && !isspace(text[end])) {
        end++;
    }
    return std::wstring(text, begin, end - begin);
}

Sentence Tokenization::getSentence(int i) {
    return sentences[i - 1];
}


int Tokenization::getNumberOfSentence() {
    return numberOfSentence;
}


void Sentence::sentenceParsing() { // Разделение предложения на слова и вставные предложения
    bool parsingWord = false;
    bool quotations = false;
    bool parenthesis = false;
    bool dash = false;

    int beginWord = 0;
    int beginInsertedSentence = 0;

    for (int i = begin; i < end; ++i) { // Проходим по всему предложению
        wchar_t c = text->at(i);
        if (iswspace(c)) { // Если встретили пробел - записываем считанное слово
            if (parsingWord) {
                words.emplace_back(Borders(beginWord, i - 1));
                parsingWord = false;
            }
        }
        if ((iswalpha(c)) || iswdigit(c)) { // проверяем что символ буква или цифра, тогда часть слова
            if (!parsingWord) {
                beginWord = i;
                parsingWord = true;
            }
        } else if (c == L'-') { // дефис (часть слова)
            if (parsingWord) {
                continue;
            } else if (iswdigit(text->at(i + 1))) { // дефис как минус при записи числа
                beginWord = i;
                parsingWord = true;
            }
        } else {
            if (parsingWord) {
                words.emplace_back(Borders(beginWord, i - 1));
                parsingWord = false;
            }
        }


        if (c == L'–') { // тире начинает и заканчивает вставное предложение
            if (dash) {
                insertedSentences.emplace_back(Borders(beginInsertedSentence, i));
                dash = false;
            } else if (!quotations && !parenthesis) {
                beginInsertedSentence = i;
                dash = true;
            }
        }
        if (c == L'"') { // кавычки начинает и заканчивает вставное предложение
            if (quotations) {
                insertedSentences.emplace_back(Borders(beginInsertedSentence, i));
                quotations = false;
            } else if (!dash && !parenthesis){
                beginInsertedSentence = i;
                quotations = true;
            }
        }
        if (c == L'(') {  // скобки начинает и заканчивает вставное предложение
            if (!dash && !quotations && !parenthesis){
                beginInsertedSentence = i;
                parenthesis = true;
            }
        }
        if (c == L')') {
            if (parenthesis) {
                insertedSentences.emplace_back(Borders(beginInsertedSentence, i));
                parenthesis = false;
            }
        }
    }
    if (parsingWord) {
        words.emplace_back(Borders(beginWord, end - 1));
    }
    if (dash || quotations || parenthesis) {
        insertedSentences.emplace_back(Borders(beginInsertedSentence, end - 1));
    }
}

std::wstring Sentence::outputFormat() { // Удобный вывод информации о предложении для отладки
    std::wstringstream res;
    res << L"Предложение: " << getSentence() << L'\n';
    res << L"Кол-во слов: " << getNumberOfWords() << L"; Кол-во предложений: " << getNumberOfInsertedSentences() << L'\n';
    if (getNumberOfInsertedSentences() > 0)
        res << L"Вставные предложения: \n";
    for (int i = 0; i < getNumberOfInsertedSentences(); ++i) {
        res << getInsertedSentence(i + 1) + L'\n';
    }
    if (getNumberOfWords() > 0)
        res << L"Слова: \n";
    for (int i = 0; i < getNumberOfWords(); ++i) {
        res << getWord(i + 1) + L'\n';
    }
    return res.str();
}
