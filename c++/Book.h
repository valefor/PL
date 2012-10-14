#include <string>
#include <map>


class Exercise {
    int number;
    std::string desc;

    void (*doIt)();

    public:
    Exercise(int);
    Exercise& setDoIt( void (*)() );
};

class Section {
    std::string sectionName;
    std::map<int,Exercise*> exercises;

    public:
    Section(std::string);
    Section & addExercise(int number,Exercise* exercise);
};

class Chapter {
    std::string chapterName;
    std::map<int,Section*> sections;

    public:
    Chapter(std::string);
    Chapter & addSection(int number,Section* section);
    
};

class Book {
    std::string bookName;
    std::map<int,Chapter*> chapters;

    public:
    Book(std::string);
    void init();
    void show();
    Book & addChapter(int number,Chapter* chapter);
};

