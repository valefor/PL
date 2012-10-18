#ifndef OPR_INCLUDED
#define OPR_INCLUDED
#include <map>
#include <iostream>

class Operable {

    Operable * parent;
    std::map<int,Operable*> oprbs;
    std::string name;

    public:
    bool executable;
    Operable * cd(int);
    void add(int,Operable *);

    Operable() {} 
    Operable(std::string name):parent(NULL),name(name),executable(false) {} 
    std::string getName() { return name; }
    Operable * getParent() { return parent; }

    virtual void show();
    virtual void execute() = 0;
    virtual ~Operable();

};

typedef std::map<int,Operable*> OperableMT;
#endif
