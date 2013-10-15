// smart pointer

template<typename Type>
class SharedPtr {

    public:
    explicit SharedPtr(Type *obj);
    SharedPtr(SharedPtr& r);
};
