#ifndef OPR_INCLUDED
#define OPR_INCLUDED

class Operable {

    public:
    bool executable;
    virtual Operable * cd(int) = 0;
    virtual void show() = 0;
    virtual ~Operable() { }

};
#endif
