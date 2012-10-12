#include <string>
#include <map>


class BookExercise {
    int number;
    std::string desc;

    void (*doIt)();

    public:
    BookExercise(int);
    void setDoIt( void (*)() );
};

class BookSection {
    std::string sectionName;
    std::map<int,BookExercise*> exercises;

    public:
    BookSection(std::string);
    void addExercise(int number,BookExercise* exercise);
};

class BookChapter {
    std::string chapterName;
    std::map<int,BookSection*> sections;

    public:
    BookChapter(std::string);
    void addSection(int number,BookSection* section);
    
};

class Book {
    std::string bookName;
    std::map<int,BookChapter*> chapters;

    public:
    Book(std::string);
    void init();
    void show();
    void addChapter(int number,BookChapter* chapter);
};

