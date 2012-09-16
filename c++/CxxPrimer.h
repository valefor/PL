#include <string>
#include <map>


class CxxPrimerExercise {
    int number;
    std::string desc;

    public:
    CxxPrimerExercise();
};

class CxxPrimerSection {
    std::map<int,std::string> index;
    std::map<int,CxxPrimerExercise*> exercises;

    public:
    CxxPrimerSection();
    void addExercise(int number,std::string name,CxxPrimerExercise section);
};

class CxxPrimerChapter {
    std::map<int,std::string> index;
    std::map<int,CxxPrimerSection*> sections;

    public:
    CxxPrimerChapter();
    void addSection(int number,std::string name,CxxPrimerSection section);
    
};

class CxxPrimer {
    std::string bookName;
    std::map<int,CxxPrimerChapter*> chapters;

    public:
    CxxPrimer(std::string);
    void show();
    void addChapter(int number,CxxPrimerChapter chapter);
};

