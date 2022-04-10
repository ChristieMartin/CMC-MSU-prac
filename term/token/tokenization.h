#ifndef TOKEN_TOKENIZATION_H
#define TOKEN_TOKENIZATION_H

#include <string>
#include <vector>

struct SentenceBorders { // Структура чтобы хранить позицию начала предложения и конца
    int begin;
    int end;
    SentenceBorders(int begin, int end)
    : begin(begin)
    , end(end) {}
};

class Tokenization { // Класс ответственный за всю токенизацию
public:
    Tokenization(const char* source, int n); // Конструктор класс
    std::string getSentence(int i); // Получить i-ое по порядку предложение
    int getNumberOfSentence(); // Получить количество предложений

    std::string getText() { // Получить изначальный текст
        return std::string(text);
    }

    ~Tokenization(); // Деструктор класса

private:
    void SentenceTokenization(); // Тут мы проходим по тексту и находим
    bool isEndOfSentence(int i); // Проверяем, является ли i-ый символ в тексте концом предложения. Пока реализовано просто через проверку точки, в дальнейшем расширить

private:
    char* text;
    int length;
    int numberOfSentence;
    std::vector<SentenceBorders> sentenceBorders; // Массив, в котором мы для каждого предложения храним начало и конец предложений
};


#endif
