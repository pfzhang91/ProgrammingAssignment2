makeCacheMatrix &lt;- function(x = matrix()) {

        inv &lt;- NULL

        set &lt;- function(y) {

                x &lt;&lt;- y

                inv &lt;&lt;- NULL

        }

        get &lt;- function() x

        setInverse &lt;- function(inverse) inv &lt;&lt;- inverse

        getInverse &lt;- function() inv

        list(set = set,

             get = get,

             setInverse = setInverse,

             getInverse = getInverse)

}

cacheSolve &lt;- function(x, ...)

        inv &lt;- x$getInverse()

        if (!is.null(inv)) {

                message(&quot;getting cached data&quot;)

                return(inv)

        }

        mat &lt;- x$get()

        inv &lt;- solve(mat, ...)

        x$setInverse(inv)

        inv

}
