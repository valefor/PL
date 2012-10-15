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
    bool cd(Operable & );
};

class Section : public Operable {
    std::string sectionName;
    std::map<int,Exercise*> exercises;

    public:
    Section(std::string);
    Section & addExercise(int number,Exercise* exercise);
    void show();
    bool cd(Operable & );
};

class Chapter : public Operable {
    std::string chapterName;
    std::map<int,Section*> sections;

    public:
    Chapter(std::string);
    Chapter & addSection(int number,Section* section);
    std::string getName() { return chapterName;}
    void show();
    bool cd(Operable & );
    
};

class Book : public Operable {
    std::string bookName;
    std::map<int,Chapter*> chapters;

    public:
    Book(std::string);
    void init();
    void show();
    bool cd(Operable & );
    Book & addChapter(int number,Chapter* chapter);
};

