import { LRUCache } from 'lru-cache'

export function uncurry2_(f)
{
    return function(a, b)
        {
            return f(a)(b);
        }
}

export function uncurry3_(f)
{
    return function(a, b, c)
        {
            return f(a)(b)(c);
        }
}

export function newCache_ (options)
    {
        return new LRUCache( options );
    }

//set(key, value, [{ size, sizeCalculation, ttl, noDisposeOnSet }])
export function set_( key, value, cache, options)
    {
        return cache.set(key, value, options);
    }

//set(key, value, [{ size, sizeCalculation, ttl, noDisposeOnSet }])
export function set__( key, value, cache)
{
    return cache.set(key, value);
}

// get(key, { updateAgeOnGet, allowStale } = {}) => value
export function get_( key, options, nothing, just, cache )
{
    const r = cache.get(key, options);
    if (r)
    {
        return just(r);
    }
    else
    {
        return nothing;
    }
}

export function size_( cache )
{
    return cache.size;
}

export function calculatedSize_( cache )
{
    return cache.calculatedSize;
}

// peek(key, { allowStale } = {}) => value
export function peek_( key, options, nothing, just, cache )
{
    const r = cache.peek(key, options);
    if (r)
    {
        return just(r);
    }
    else
    {
        return nothing;
    }
}

// has(key, { updateAgeOnHas } = {}) => Boolean
export function has_(key, options, cache)
{
    const r = cache.has(key, options);
    if (r)
    {
        return true;
    }
    else
    {
        return false;
    }
}

export function delete_(key, cache)
{
    return cache.delete(key);
}

export function clear_(unit, cache)
{
    cache.clear();
    return unit;
}

export function keys_(cache)
{
    return cache.keys();
}

export function rkeys_(cache)
{
    return cache.rkeys();
}

export function values_(cache)
{
    return cache.values();
}
export function rvalues_(cache)
{
    return cache.rvalues();
}

export function entries_( tuple, cache )
{
    const entryIterator = cache.entries();
    return {
        next: function()
            {
                const n = entryIterator.next();
                if (n.done)
                {
                    return n;
                }
                else
                {
                    n.value = tuple( n.value[0]) (n.value[1]);
                    return n;
                }
            }
    }
}

export function rentries_( tuple, cache )
{
    const entryIterator = cache.rentries();
    return {
        next: function()
            {
                const n = entryIterator.next();
                if (n.done)
                {
                    return n;
                }
                else
                {
                    n.value = tuple( n.value[0]) (n.value[1]);
                    return n;
                }
            }
    }
}

export function find_( criterium, options, nothing, just, cache )
{
    const r = cache.find( criterium, options );
    if (r)
    {
        return just(r);
    }
    else
    {
        return nothing;
    }
}

export function dump_( cache )
{
    return cache.dump();
}

export function load_( keysAndVals, cache )
{
    return cache.load( keysAndVals );
}

export function purgeStale_( cache )
{
    return cache.purgeStale();
}

export function getRemainingTTL_( key, nothing, just, cache )
{
    const r = cache.getRemainingTTL( key );
    if (r === Infinity)
    {
        return nothing;
    }
    else
    {
        return just(r);
    }
}

export function forEach_( fn, unit, cache)
{
    cache.forEach( (val, key, cache) => fn(val, key, cache)() );
    return unit;
}

export function rforEach_( fn, unit, cache)
{
    cache.rforEach( (val, key, cache) => fn(val, key, cache)() );
    return unit;
}

export function pop_( nothing, just, cache )
{
    const r = cache.pop();
    if (r)
    {
        return just(r);
    }
    else
    {
        return nothing;
    }
}

export function logDisposal( item )
{
    return function( key )
    {
        return function( reason )
            {
                console.log( "Disposing of item with key " + key + " because: " + reason);
            }
    }
}