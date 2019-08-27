
# Multicast events using generics

| Article | |
| --- | ---|
| Author | Allen Bauer |
| Date | 15 Aug 2008 |
| Source | https://community.embarcadero.com/blogs/entry/multicast-events-using-generics-38865 |

Ever since before Delphi 1, the (then Delphi only) RAD Studio IDE has been full of home-grown multicast event class types. I usually refer to these as an "event bus." This is from my hardware days when I designed micro-controller based security/access control equipment. A CPU has an "address bus" and a "data bus" which can have one sender and any number of "listeners," not unlike a multicast event.

I've been following with interest these series of posts about creating a multicast event class in native Delphi. What struck me was that the class being presented was remarkably similar to the class and its descendants that we've used in the RAD Studio IDE since before Delphi 1. However the one presented in those posts does take it to another level and adds some extra housekeeping to notify the listener and/or the multicast event when each other is being freed. While that's a nice addition, it does tend to complicate its use. From my point of view, regardless of whether you have a single-cast or multicast event, you should always balance assigning the event with clearing the event. IOW, make sure the event isn't pointing to a dead instance. Overall, what was presented is a pretty decent design given what the Delphi language has to offer today.

One of the key new Delphi language features for Tiburón (Delphi 2009) is the introduction of generics. During the development cycle, I began thinking if there was a way to create a multicast event class using generics. At first glance this seemed like an easy thing to do. After all, a multi cast event class is just a wrapper around an array of method pointers. To send out the event you iterate through the array of method pointers invoking each event in turn. So surely it's as simple as this:

```pas
type
  TMulticastEvent<T> = class
  private
    FHandlers: array of T;
  public
    procedure Add(Handler: T);
    procedure Remove(Handler: T);
    procedure Invoke(<uh oh>);
  end;
```

Hmm... Do you see the problem? How can I actually invoke the event? Adding and removing handlers looks straight forward, but invoking events is a problem. The multicast event class inside the Delphi IDE starts with a base class that handles the array of method pointers and it is up to the manually created descendants to add the Add and Remove methods and the specific Invoke (or Send in our case) method that provides the right parameter list and does the job of iterating through the assigned method pointers. Lots of ugly typecasting was happening in addition to having to manually create the descendants.

Ok, now I have a challenge. The gauntlet has been thrown. I'm going to design a generic multicast event using Delphi native code, dagnabbit! The end result still looked very similar to the internal IDE multicast event classes. The way I solved the invoke problem was to declare a property of type T.

```pas
type
  TMulticastEvent<T> = class
  private
    FInvoke: T;
    // ... <same above as>
    property Invoke: T read FInvoke;
  end;
```

Syntactically you just call Invoke like any old method:

```pas
type
  TMyEvent = procedure (Sender: TObject; IntValue: Integer; FloatValue: Double) of object;
  MulticastMyEvent: TMulticastEvent<TMyEvent>
begin
  MulticastMyEvent := TMulticastEvent<TMyEvent>.Create;
  MulticastMyEvent.Invoke(Obj, 10, 3.14);
end.
```

There is still an interesting problem here. Do you see it? So I can use the event property "simulate" a method, but what is assigned to this method pointer? Where does control go when you call through this method pointer? When is is assigned? This is when things started getting a little interesting.

The problem with all the other multicast event class implementations I've seen (including our own internal one) is that for type-safety they require you to manually create a descendant class with the actual type of the event. It's tedious and error prone, and promotes gross duplication of code. These are the problems that generics are suppose to solve, right? However in this instance, it was almost like I needed to instantiate code at run-time (something that .NET actually does which solves many of these problems). Barring that, I had to reach a little deeper into my bag-o-tricks. All the multicast event classes share a couple of things; maintain a list or array of method pointers and iterate over that list and invoke each method. All method pointers are the same size (8 bytes consisting of a pointer to an object instance and a pointer to the method to invoke). I created a non-generic base class much like the existing base class we've had for years:

```pas
type
  TMulticastEvent = class
  strict protected type TEvent = procedure of object;
  strict private
    FHandlers: array of TMethod;
    procedure Add(const AMethod: TEvent); overload;
    prodedure Remove(const AMethod: TEvent); overload;
    function IndexOf(const AMethod: TEvent): Integer; overload;
  end;
```

Hmmmm... Why is the array declared here instead of the descendant? How will this be "type-safe?" You can't cast an arbitrary "T" to a TMethod. Oh and waidaminnit... the Add, Remove and IndexOf methods are private! To address these issues, I pulled out some assembler tricks out of my proverbial bag and I added the following protected methods.

```pas
type
  TMulticastEvent = class
    // ... <stuff from above>
  protected
    procedure InternalAdd;
    procedure InternalRemove;
    procedure InternalIndexOf;
  end;
```

Uh, dude, there are no parameters on those methods. What do they do? Here's the method building from the InternalAdd method. The others are very similar.

```
procedure TMulticastEvent.InternalAdd;
asm
       XCHG  EAX,[ESP]
       POP   EAX
       POP   EBP
       JMP   Add
end;
```


What this function does is removes itself and the immediate caller from the call chain and directly transfers control to the corresponding "unsafe" method while retaining the passed in parameter(s). As we'll see later on, we're going to create a generic descendant class from this one where the method bodies of the actual type-safe versions of Add, Remove and IndexOf merely call the corresponding "InternalXXX' versions from this base class. Here's some of what that descendant will look like and the building of the Add method:

```
type
  TMulticastEvent<T> = class(TMulticastEvent)
  public
    procedure Add(const AMethod: T); overload;
    procedure Remove(const AMethod: T); overload;
    function IndexOf(const AMethod: T): Integer; overload;
  end;

procedure TMulticastEvent.Add(const AMethod: T);
begin
  InternalAdd;
end;
```


Ah, there's the "T"... It's beginning to look pretty generic. Why do it this way? The primary reason is that you cannot have any assembly code in the method building of any generic type or method, but you can refer to other methods that are written in assembly. So in this case the ancestor class merely provides those "non-generic-able" assembler functions. Another reason, which we'll get to, is that I also want to make sure that iterating through the array of event handlers is also done in some common code, which will also need to be in assembly due to some more stack tricks that must be done.

When this generic class is instantiated with an event type, the Add, Remove, and IndexOf methods will always get a stack frame. Even though the method building doesn't touch the parameter, the frame is still there and needs to be cleaned up because the size of the event type is > 4 bytes. Since it wont fit in a register, it is always passed by value on the stack. This is also why I declared a private TEvent type up in the base class so that the stack pattern will match. If I had merely declared the parameters as a TMethod, which is a record that allows you to "crack open" the event type, it would not have worked. This is because method pointers are passed differently from records because you can directly refer to a method and instance in code as the parameter when calling a method that takes a method pointer:

```pas
begin
  ...
  MC.Add(Obj.EventHandler);
  ...
end;
```


In this case the compiler simply pushes the instance reference from the variable Obj onto the stack, followed by the address of the EventHandler method. However for a record, the compiler passes a pointer to the record, which may even be in a register. Then, depending upon whether or not the parameter is declared as "const" or not, the called method will use the pointer directly or make a local copy of the record on the stack.

I won't bore you with the details of how the Add, Remove, and IndexOf methods actually interact with the array as that really isn't the interesting part and is merely boilerplate code anyway. Let's instead turn our attention to triggering or invoking the event. Let's add back in the FInvoke field and the Invoke property:

```pas
type
  TMulticastEvent<T> = class(TMulticastEvent)
  strict private
    FInvoke: T;
  public
    procedure Add(const AMethod: T); overload;
    procedure Remove(const AMethod: T); overload;
    function IndexOf(const AMethod: T): Integer; overload;
    property Invoke: T read FInvoke;
  end;
```

So here we are again. What is assigned to the FInvoke field? This is where yet another trick from my bag comes out. We're going to leverage some of the dynamic dispatching code that has been laying somewhat dormant down in the ObjAuto.pas unit for many Delphi releases. If you haven't had the chance to look at some of the little gems that live in that unit and the associated ObjComAuto.pas, there is some pretty powerful bits of code in there. ObjAuto's primary purpose is to allow late-bound calling of object methods. It does this through some rather rich RTTI that the compiler has generated for several releases. A lot of this extra information is generated for public and published methods on classes and descendants that have been declared within a {$METHODINFO ON} block. However, for method pointer types, this information is always available regardless of that setting. While I'm not going to be generating code at runtime, I will be calculating some information about call signature of the method at runtime from this extra RTTI.

In ObjAuto, there is an interesting global function called CreateMethodPointer(). This function takes a pointer to a type data record (the compiler RTTI) and an interface. It returns a TMethod record that points to a stub object and method that knows how to "pick apart" the passed in parameters convert them to Variants and then call some methods on the interface. Don't worry, we're not going down that (using Variants) road! Down in the implementation of this unit was something much closer to what I wanted. I just need raw data structure that was an opaque representation of the Invoke call. I don't care what the data is, only how large it is and where it is located. IOW, what data is in a register and what data is on the stack and how much. I won't go into the raw details of the changes made down in ObjAuto since you'll be able to see that once Tiburón ships. I added another overloaded CreateMethodPointer() function, that takes a method pointer callback instead of an interface and a pointer to a type data record. Here's what the declarations look like:

```pas
type
  ...
  PParameters = ^TParameters;
  TParameters = packed record
    Registers: array[paEDX..paECX] of Cardinal;
    Stack: array[0..1023] of Byte;
  end;
  TDynamicInvokeEvent = procedure (Params: PParameters; StackSize: Integer) of object;

function CreateMethodPointer(const ADynamicInvokeEvent: TDynamicInvokeEvent; TypeData: PTypeData): TMethod; overload;
procedure ReleaseMethodPointer(MethodPointer: TMethod);
```


Call CreateMethodPointer with a reference to a method of type TDynamicInvokeEvent and the type data for the method pointer (T). It will return a TMethod record, which is essentially a raw method pointer. We'll use this value to assign to FInvoke. The trick is to get around the fact that the compiler won't allow this cast:

```pas
  FInvoke := T(CreateMethodPointer(...)); // compiler chokes on this code
```


We have to resort to another assembler trick. Remember how I said that passing a method pointer as a parameter is a little different that with records? In this case, it's not going to matter because we're going to declare a method in the generic class that takes a "var" parameter. In this case the compiler passes the address of the method pointer variable rather than the value. Let's add this method:

```pas
type
  TMulticastEvent<T> = class(TMulticastEvent)
  strict private
    FInvoke: T;
    procedure SetEventDispatcher(var ADispatcher: T; ATypeData: PTypeData);
  public
    // ... < same as above >
  end;

procedure TMulticastEvent.SetEventDispatcher(var ADispatcher: T; ATypeData: PTypeData);
begin
  InternalSetDispatcher;
end;
```

There's another "InternalXXXX" call! That one is only slightly different than the others, but the effect is the same. InternalSetDispatcher simply jumps over to the SetDispatcher method on the TMulticastEvent type, which is declared and implemented on the base TMulticastEvent class:

```pas
type
  TMulticastEvent = class
  strict protected type TEvent = procedure of object;
  strict private
    FHandlers: array of TMethod;
    FInternalDispatcher: TMethod; // this class needs to keep it's own reference for cleanup later
    // ...
    procedure InternalInvoke(Params: PParameters; StackSize: Integer);
    procedure SetDispatcher(var AMethod: TMethod; ATypeData: PTypeData);
  protected
    // ...
    procedure InternalSetDispatcher;
  end;

procedure TMulticastEvent.SetDispatcher(var AMethod: TMethod; ATypeData: PTypeData);
begin
  if Assigned(FInternalDispatcher.Code) and Assigned(FInternalDispatcher.Data) then
    ReleaseMethodPointer(FInternalDispatcher);
  FInternalDispatcher := CreateMethodPointer(InternalInvoke, ATypeData);
  AMethod := FInternalDispatcher;
end;
```

We're almost there. One of the last bits to cover is the InternalInvoke method and the constructor of the generic TMulticastEvent class. InternalInvoke is called from the little stub class instance created by the CreateMethodPointer() call. It passes in the pointer to the TParameters record and the space taken up on the stack by the parameters that "spill" from the CPU registers. As you may have already guessed, we're going to drop down to some more assembly:

```pas
procedure TMulticastEvent.InternalInvoke(Params: PParameters; StackSize: Integer);
var
  LMethod: TMethod;
begin
  for LMethod in FEvents do 
  begin 
    // Check to see if there is anything on the stack. 
    if StackSize > 0 then 
      asm
        // if there are items on the stack, allocate the space there and 
        // move that data over. 
        MOV ECX,StackSize
        SUB ESP,ECX 
        MOV EDX,ESP
        MOV EAX,Params 
        LEA EAX,[EAX].TParameters.Stack[8]
        CALL System.Move 
      end; 
    asm
      // Now we need to load up the registers. EDX and ECX may have some data 
      // so load them on up.
      MOV EAX,Params 
      MOV EDX,[EAX].TParameters.Registers.DWORD[0] 
      MOV ECX,[EAX].TParameters.Registers.DWORD[4]
      // EAX is always "Self" and it changes on a per method pointer instance, so 
      // grab it out of the method data. 
      MOV EAX,LMethod.Data 
      // Now we call the method. This depends on the fact that the called method 
      // will clean up the stack if we did any manipulations above. 
      CALL LMethod.Code 
    end; 
  end; 
end;
```

Now the constructor for the generic version of TMulticastEvent<T>:

```pas
constructor TMulticastEvent<T>.Create;
var
  MethInfo: PTypeInfo;
  TypeData: PTypeData;
begin
  MethInfo := TypeInfo(T);
  TypeData := GetTypeData(MethInfo);
  inherited Create;
  Assert(MethInfo.Kind = tkMethod, 'T must be a method pointer type');
  SetEventDispatcher(FInvoke, TypeData);
end;
```

Notice the Assert, that is there because we cannot "constrain" the generic type at compile-time to only accept method pointer types for "T". So we have to do that check at run-time. This is something we'll look at for future releases. We can safely call SetEventDispatcher because FInvoke will be of type "T".

There you have it, a generic multicast event. At some point, I'll present some extensions that will help make using this easier by adding some automatic cleanup of the multicast event instance itself and a technique for using the source event itself to be the only instance reference. For now here's one potential use scenario:

```pas
procedure TForm1.FormCreate(Sender: TObject);
var
  E: TMulticastEvent<TNotifyEvent>;
begin
  E := TMulticastEvent<TNotifyEvent>.Create;
  // Since the Invoke property is of type "T" and T = TNotifyEvent, you can
  // Assign it directly to any event of type TNotifyEvent, such as OnClick.
  Button1.OnClick := E.Invoke;  
  // Now you can add as many "listeners" as you want.
  E.Add(Button1Click);
  E.Add(AnotherButtonClick);
end;
```

I will try to get the full code posted within the next few weeks or so. I still have Tiburón to get out the door and take a few days off.

There are also a lot of caveats and cases where a multicast event can really mess things up. For instance, what if the method pointer had a result type? What if it had one or more "var" or and "out" parameters? Which method invocation in the list of listening handlers decides what the result is, or assigns to an "out" parameter? What if an exception is raised? Are there any ordering dependencies or expectations? From a framework designer standpoint, these are some real questions one has to ask. Making all events multicast is akin to the shotgun approach of making all methods virtual. It doesn't really foster extensibility in the way one would think. As a matter of fact it can serve to severely limit extensibility and evolve-ability of a given framework.

There are many types of events that lend themselves very well to supporting multicasting, and there are plenty of other cases where multicasting only creates more problems than it solves. Having multicast events as the default behavior for any event type is certainly nice from a consistency and overall availability standpoint, but it just makes it way too easy to hang yourself inadvertently. Supporting multicasting for an event source should be a decision left to the designer of the class on which the event is placed.