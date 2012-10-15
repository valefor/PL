class Operable {

    public:
    bool executable;
    virtual bool cd(Operable & oprb) = 0;
    virtual void show() = 0;
    ~Operable() { }

};
