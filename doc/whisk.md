

# Module whisk #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td>
Connect to a server.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>
Delete a key/value pair.</td></tr><tr><td valign="top"><a href="#disconnect-1">disconnect/1</a></td><td>
Disconnect from the server.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>
Get the value associated with the key.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Equivalent to <a href="#set-3"><tt>set(Key, Value, [{expiry, 0}, {cas, 0}])</tt></a>.</td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td>
Set the key-value pair.</td></tr><tr><td valign="top"><a href="#stat-1">stat/1</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#stats-1">stats/1</a></td><td>
Collect the stats for the server.</td></tr><tr><td valign="top"><a href="#timeout-2">timeout/2</a></td><td>
Set socker timeout.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>
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

<a name="delete-2"></a>

### delete/2 ###

<pre><code>
delete(Pid::pid(), Key::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete a key/value pair

<a name="disconnect-1"></a>

### disconnect/1 ###

<pre><code>
disconnect(Pid::pid()) -&gt; ok | {error, term()}
</code></pre>
<br />

Disconnect from the server.

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Pid::pid(), Key::term()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Get the value associated with the key.

<a name="set-3"></a>

### set/3 ###

<pre><code>
set(Pid::pid(), Key::term(), Value::term()) -&gt; {ok, integer()} | {error, term()}
</code></pre>
<br />

Equivalent to [`set(Key, Value, [{expiry, 0}, {cas, 0}])`](#set-3).

<a name="set-4"></a>

### set/4 ###

<pre><code>
set(Pid::pid(), Key::term(), Value::term(), Options::[{atom(), term()}]) -&gt; {ok, integer()} | {error, term()}
</code></pre>
<br />

Set the key-value pair

<a name="stat-1"></a>

### stat/1 ###

<pre><code>
stat(Pid::pid()) -&gt; {ok, [{binary(), binary()}]} | {error, term()}
</code></pre>
<br />

__This function is deprecated:__ Please use [`whisk:stats/1`](whisk.md#stats-1)

<a name="stats-1"></a>

### stats/1 ###

<pre><code>
stats(Pid::pid()) -&gt; {ok, [{binary(), binary()}]} | {error, term()}
</code></pre>
<br />

Collect the stats for the server.

<a name="timeout-2"></a>

### timeout/2 ###

<pre><code>
timeout(Pid::pid, Timeout::integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

Set socker timeout

<a name="version-1"></a>

### version/1 ###

<pre><code>
version(Pid::pid()) -&gt; {ok, binary()} | {error, term()}
</code></pre>
<br />

Version of the memcache server.

