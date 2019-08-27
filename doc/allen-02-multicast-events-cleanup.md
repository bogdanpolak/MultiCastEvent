# Multicast Events - the cleanup

| Article | |
| --- | ---|
| Author | Allen Bauer |
| Date | 25 Aug 2008 |
| Source | https://community.embarcadero.com/blogs/entry/multicast-events--the-cleanup-38866 |

In my last post, I introduced a multicast event that uses generics to address the problem of needing to manually declare and implement a new multicaster for each unique event type. If you remember, I referred to some other posts that presented a technique for doing automatic cleanup of both the multicast event object itself and automatic removal of listeners. While the technique presented is indeed very generic and will work for nearly all instances, my only critique is that it carries a fairly heavy "contract" in order to fully realize its potential. This set all the creaky wheels moving as I tried to come up with something a little less "contract" heavy. This solution has an assumption that most events and event handlers are on TComponent derived classes. Yes, there are plenty of folks out there that use method pointers on object instances of types other than TComponent. I'm using the 80/20 rule. This will be highly useful to 80% of you out there and not as useful for the remaining 20%.

One of the main problems of a "bolt-on" multicast event class in Delphi for Win32 is proper cleanup. With normal single-cast events there is but one sender and one listener. The end-points are already presumed to be lifetime managed. For multicast events, no longer is there a point-to-point connection, but rather an intermediate "agent" that serves to translate a single event into "n" events for "n" listeners. In a nice garbage collected run-time environment, this isn't a problem because this intermediary will be properly cleanup in due time. This is not meant to be a ding against the inherent non-GC Delphi/Win32 environment but merely that we need to be a little more creative. The good thing here is that once we've established this solution, you can return to your "regularly scheduled programming" and no longer worry about this cleanup.

Let's start with a little review. TComponent and its derivatives carry the notion of "ownership" for the exact reason of not being able to rely on a GC. This model was established from the very first introduction of Delphi and the VCL. Any component that has an "owner" will be automatically cleaned up when its owner was cleaned up. In order for this to operate effectively, there was a need for some mechanism to let everyone involved know when an instance was going away. This is the reason for the Notification virtual method on TComponent. Every component that is being cleaned up, or more accurately, being removed from the list of "owned" components is sent broadcast to all the other owned components through this Notification method with the "opRemove" operation enumeration value. Initially, this notification only worked for components that shared a common "owner," which worked well for Delphi 1 and 2. In Delphi 3, form-inheritance and form-linking where introduced which allowed cross-form/data-module references between components. For instance, the TTable component on a data-module could now be referenced from a TDataSource component on a form. This presented a problem because the data-module and form could have very different lifetimes and the previously mentioned notification mechanism just doesn't work. This is when the FreeNotification() method was introduced on TComponent. This allows any component to insert themselves into the "free notification" list of another component in order to "link" them together and know when each other is going away. Now proper cleanup (such as setting references to nil) can be done even for components that do not share the same owner. We can use this knowledge to devise a solution for properly cleaning up a multicast event object.

I created the following generic descendant of TMulticastEvent<T>:

```pas
type
  TComponentMulticastEvent<T> = class(TMulticastEvent<T>)
  private type
    TNotificationSink = class(TComponent)
    private
      FEvent: TMulticastEvent;
      FOwnerComp: TComponent;
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AOwner: TComponent; AEvent: TMulticastEvent); reintroduce;
      destructor Destroy; override;
    end;
  public
    constructor Create(AOwner: TComponent);
  end;
```

Since the TMulticastEvent ancestor isn't a TComponent derivative I needed a way to "listen in" on the notifications sent between TComponents. This is why there is a private component derived from TComponent which provides the link between the multicast event instance and the component notifications. I could have simply made this private component owned by the component and not even worried about the Notification method because all owned components are properly cleaned up. However, the intent is that this internal component needs to be invisible (relatively speaking) and not show up in the list of owned components. A lot of code out there iterates through the list of owned components and performs some operation on each one. Don't want to risk breaking that code. So, when this private component is instantiated, it isn't "owned" by any component but by using FreeNotification, it will still know when the "owner" component is freed.

In order to make it a little easier to instantiate these multicast events I added the following public class static methods to the base non-generic TMulticastEvent class:

```pas
type
  TMulticastEvent = class
    ...
  public
    ...
    class function MulticastEvent<T>(const AMethod: T): TMulticastEvent<T>; static;
    class function Create<T>(AComponent: TComponent): T; overload; static;
    class function Create<T>: T; overload; static;
  end;
```

The first one we'll come back to in a moment. The two overloaded Create methods will create a TComponentMulticastEvent<T> or simply a TMulticastEvent<T>, respectively. If you'll notice that the multicast event instance is not being returned, but rather a value of the method pointer type itself is returned. This is the Invoke property. You can now simply do this:

```pas
procedure TForm1.Form1Create(Sender: TObject);
begin
  Button1.OnClick := TMulticastEvent.Create<TNotifyEvent>(Button1);
end;
```

Notice how that looks remarkably similar to a normal object instantiation? Cool. Uh... waitaminnit. How do I add listeners? I cannot call Add, or Remove now. One solution would be to first create the event, add the listeners, then assign the Invoke property to the OnClick event like this:

```pas
procedure TForm1.Form1Create(Sender: TObject);
var
  Click: TMulticastEvent<TNotifyEvent>;
begin
  Click := TComponentMulticastEvent<TNotifyEvent>.Create(Button1);
  Click.Add(Button1Click);
  Click.Add(Button2Click);
  Button1.OnClick := Click.Invoke;
end;
```

Yes, that would work, but only if you know up front what the listeners would be and that the list won't ever change once it is setup. The beauty of multicast events is that they're dynamic and can change at run-time. Remember the first class static method listed above? This is where that cute little method comes into play. Let's redo the above code using that method:

```pas
procedure TForm1.Form1Create(Sender: TObject);
begin
  Button1.OnClick := TMulticastEvent.Create<TNotifyEvent>(Button1);
  TMulticastEvent.MulticastEvent<TNotifyEvent>(Button1.OnClick).Add(Button1Click);
  TMulticastEvent.MulticastEvent<TNotifyEvent>(Button1.OnClick).Add(Button2Click);
end;
```

While the above techniques are equivalent in this instance, what if the addition or removal of the listeners occurred elsewhere? Instead of keeping a separate reference to the multicast event instance, it is simply fished out of the method pointer value itself. Also, because we know it too will be cleaned up, we can now simply concentrate on using it.

On a final note, the above implementation doesn't currently support automatic removal of listeners when they go away, but that can certainly be done. We'll look at that in the next installment. Your homework is this; using the class static methods above to in-turn implement class static Include() and Exclude() methods that will mimic the Include/Exclude standard functions in Delphi for .NET.
