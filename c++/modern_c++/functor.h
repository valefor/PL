/*
 * Functor
 *  Note C++11 has introduced variadic template,so template declaration like this
 *      template <typename ... Args> class Variant; 
 *  is definitely legal.
 */

template <typename ReturnType>
class Functor
{
    // implementation

    public:
    ReturnType operator() ();
};
