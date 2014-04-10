/*
 * Functor
 *  Note C++11 has introduced variadic template,so template declaration like this
 *      template <typename ... Args> class Variant; 
 *  is definitely legal.
 */

#include <memory>
#include "typelist.h"

using TL::TypeList;
using TL::TypeAt;

template <typename ReturnType, class TList>
class FunctorImpl;

// Specialization
template <typename R>
class FunctorImpl<R, NullType>
{
    virtual R operator () () = 0;
    virtual FunctorImpl * clone() const;
    virtual ~FunctorImpl () {};
};

template <typename R, typename P1>
class FunctorImpl<R, TYPELIST1(P1)>
{
    virtual R operator () (P1) = 0;
    virtual FunctorImpl * clone() const;
    virtual ~FunctorImpl () {};
};

template <typename R, typename P1, typename P2>
class FunctorImpl<R, TYPELIST2(P1,P2)>
{
    public:
    virtual R operator () (P1, P2) = 0;
    virtual FunctorImpl * clone() const = 0;
    virtual ~FunctorImpl () {};
};

template <typename Functor, typename Func>
class FunctorHandler : 
    public FunctorImpl<
        typename Functor::ResultType,
        typename Functor::ParmList
        >
{
    public:
    typedef typename Functor::ResultType ReturnType;

    FunctorHandler(const Func& func) : func_(func) {}

    FunctorHandler * clone() const {
        return new FunctorHandler(*this);
    }
    
    ReturnType operator() () { return func_(); }
    ReturnType operator() (typename Functor::Parm1 p1) { return func_(p1); }
    ReturnType operator() (typename Functor::Parm1 p1,
        typename Functor::Parm2 p2) { return func_(p1,p2); }

    private :
    Func func_;
};

template <typename ReturnType, class TList>
class Functor
{
    private:
    //typedef typename TList::At<0>::type Parm1;
    //typedef typename TList::At<1>::type Parm2;
    typedef FunctorImpl<ReturnType, TList> Impl;
    std::auto_ptr<Impl> impl_;

    public:
    typedef TList ParmList;
    typedef ReturnType ResultType;
    typedef typename TypeAt<TList,0>::result Parm1;
    typedef typename TypeAt<TList,1>::result Parm2;
    template <typename Func>
    Functor(const Func& func);
    Functor(const Functor&);
    Functor& operator=(const Functor&);
    explicit Functor(std::auto_ptr<Impl> impl);
    ReturnType operator() () { return (*impl_)();}
    ReturnType operator() (Parm1 p1) { return (*impl_)(p1);}
    ReturnType operator() (Parm1 p1, Parm2 p2) { return (*impl_)(p1, p2);}
};

template <typename ReturnType, class TList>
template <typename Func>
Functor<ReturnType,TList>::Functor(const Func& func) : impl_(new FunctorHandler<Functor,Func>(func))
{
}
