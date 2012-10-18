#include <string>
#include <map>
#include "Interface.h"

class Exercise : public Operable {
    int number;
    std::string desc;

    void (*doIt)();

    public:
    Exercise(int);
    Exercise& setDoIt( void (*)() );
    void show();
    void execute();
};

class Section : public Operable {
    public:
    Section(std::string);
    Section & addExercise(int number,Exercise* exercise);
    void show();
    void execute();
};

class Chapter : public Operable {
    public:
    Chapter(std::string);
    Chapter & addSection(int number,Section* section);
    void show();
    void execute();
    
};

class Book : public Operable {
    public:
    Book(std::string);
    void show();
    void execute();
    Book & addChapter(int number,Chapter* chapter);
};

