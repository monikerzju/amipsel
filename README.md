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

（石）




## 拓扑

（苑）