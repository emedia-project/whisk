

# Module whisk #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td>
Connect to a server.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>
Delete a key/value pair.</td></tr><tr><td valign="top"><a href="#disconnect-0">disconnect/0</a></td><td>
Disconnect from the server.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>
Get the value associated with the key.</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>Equivalent to <a href="#set-3"><tt>set(Key, Value, [{expiry, 0}, {cas, 0}])</tt></a>.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>
Set the key-value pair.</td></tr><tr><td valign="top"><a href="#stat-0">stat/0</a></td><td></td></tr><tr><td valign="top"><a href="#stats-0">stats/0</a></td><td>
Collect the stats for the server.</td></tr><tr><td valign="top"><a href="#version-0">version/0</a></td><td>
Version of the memcache server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="connect-2"></a>

### connect/2 ###

<pre><code>
connect(Host::string(), Port::integer()) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

Connect to a server.

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(Key::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete a key/value pair

<a name="disconnect-0"></a>

### disconnect/0 ###

<pre><code>
disconnect() -&gt; ok
</code></pre>
<br />

Disconnect from the server.

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Key::term()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Get the value associated with the key.

<a name="set-2"></a>

### set/2 ###

<pre><code>
set(Key::term(), Value::term()) -&gt; {ok, integer()}
</code></pre>
<br />

Equivalent to [`set(Key, Value, [{expiry, 0}, {cas, 0}])`](#set-3).

<a name="set-3"></a>

### set/3 ###

<pre><code>
set(Key::term(), Value::term(), Options::[{atom(), term()}]) -&gt; {ok, integer()}
</code></pre>
<br />

Set the key-value pair

<a name="stat-0"></a>

### stat/0 ###

<pre><code>
stat() -&gt; {ok, [{binary(), binary()}]}
</code></pre>
<br />

<a name="stats-0"></a>

### stats/0 ###

<pre><code>
stats() -&gt; {ok, [{binary(), binary()}]}
</code></pre>
<br />

Collect the stats for the server.

<a name="version-0"></a>

### version/0 ###

<pre><code>
version() -&gt; {ok, binary()}
</code></pre>
<br />

Version of the memcache server.

