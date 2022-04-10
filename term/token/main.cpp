#include <iostream>
#include <string>

#include "tokenization.h"

int main() {
    std::string str = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.";
//    std::cin >> str;

    Tokenization tokenization(str.c_str(), str.length());

    std::cout << "Original text: " + tokenization.getText() << std::endl;
    std::cout << "Number of sentences: " << tokenization.getNumberOfSentence() << std::endl;
    std::cout << "Sentences:" << std::endl;
    for (int i = 1; i <= tokenization.getNumberOfSentence(); ++i) {
        std::cout << i << ") " << tokenization.getSentence(i) << std::endl;
    }

    return 0;
}
