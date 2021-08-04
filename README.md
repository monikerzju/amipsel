AMIPSEL
=======================



## 前端

前端采用2发射设计，最大发射数量可配置。

### 时序亮点



## 后端

后端采用3发射、3写回设计，可配置最大发射数量。后端主要由ISQ、IssueArbiter、ALU、MDU、LU、SU、CP0、RegFile组成。

后端有IS、EX、WB这3段，目前EX段有ALU、MDU、LU和SU。

### 功能部件介绍

### 流水线发射策略

（沈）

### 流水线控制

流水线控制主要解决的问题是stall、forwarding、bubble和redirect的问题。此外，异常处理机制和ds也是AMIPSEL需要处理的特殊情况。

Stall的来源目前有MDU和D-Cache，这些stall都在EX段。它们保持住EX段寄存器内容不变，IS段不发射新的指令（也就是ISQ不dequeue），同时向WB段寄存器插入bubble。

Forwarding只做了2个ALU的在WB段的结果对4个FU的forwarding，LU在WB的结果不对EX段进行forwarding，而是通过IssueArbiter限制住新指令的发射，也就是说，当EX段有load的rd和IS段的待发射指令的rs、rt冲突时，IssueArbiter拒绝发这条指令，从而可能会形成1个周期的stall。Forwarding，从哪个FU forwarding是在IS判断，具体的forwarding过程在EX段。

Bubble是气泡的插入，在不发射指令，或上一段的指令没有全部执行完的时候，都会插入bubble。

Redirect有2种，一种是CP0发出的，另一种是branch和jump，该注意的就是，当kill发出时，就不能在EX段访存了（storeValid信号有体现这一点）。

异常处理机制。异常处理，分为FIFO之前的异常和在EX段才能检测出的异常。FIFO之前的异常有`保留指令、Trap指令、指令缺页`，这些的做法都是发射的时候都只发射这一条指令以及前面的指令到对应的FU，这样它们到WB就可以直接杀掉此时在EX段的指令。更多异常是在EX段捕获，每一个FU都有其他模块的异常检测（这对时序提出了挑战，多余的硬件添加是有意义的），检测完成后除了SU会立马封住当前的cache访问，也不会做任何事情，这等到WB段才会禁止写回，也就是说，除了异常的指令和前面的指令，后面的指令都被认为是不valid的。这相当于杜绝所有异常之后的写操作。这里特别需要注意的是，跳转地址异常的指令，是跟着IF的而不是跳转指令本身，也就意味着，如果没有跳转，那么不会触发这个异常，这点和RISC-V非常不一样。

中断机制。

延迟槽机制。

延迟槽与异常结合起来。

## 高速缓存

dcacche：直连，16KB，写分配，行位宽256 bit。
icache：直连，8KB，行位宽256 bit.

输入地址为虚地址，在内部执行映射，并在tag中存储物理地址。


存储介质统一使用block ram，输入有效地址到后会延迟一拍返回数据。为消化访问延迟， cache 分为2个流水段，第一个周期将传入的地址映射后输入block ram，在第二个周期返回命中结果和有效数据，并在数据有效时发送response valid信号。在response valid信号的同一个周期dcache开始响应新的请求。

从外部看来，在周期0输入有效请求后，期望在周期1收到response valid信号并开始发送新的请求，否则表示周期0的访问未命中并进入cache stall。



收到request valid 的下一个周期开始，cache不再响应请求信号的变化，直到发送response vaild的同一个周期重新进入响应状态。在此期间内cache将最后收到的有效请求的各个字段暂存，不要求发送端维持信号。


若请求为写，由于行位数大于字长，需要2次block ram访问：第一次读出整条cacheline，第二次将修改后的cacheline写入。为了使写操作流水化，硬件资源至少要能同时一次读和一次写。实现中使用的是true dual port block ram，连续写命中时两个端口分别作为读口和写口使用。若检测到上一请求的写地址和此次请求的读地址冲突，则将上一条请求写入的数据直接定向到读请求。

同理，metadata在写命中时也需要判断命中和修改脏位两次操作，处理方式相同。


状态机的设计较为简单。判断和响应命中请求在normal状态完成；若未命中则根据meta返回的脏位决定是否写回；最后重新填充cacheline。
出于时序和路径长度的考虑，收到axi响应时并不直接将数据传回核内，而是加入一个缓冲状态resp_valid，用2拍完成从axi crossbar到核内的传输。





## 拓扑

（苑）
