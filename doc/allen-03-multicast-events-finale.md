# Multicast Events - the finale

| Article | |
| --- | ---|
| Author | Allen Bauer |
| Date | 3 Sep 2008 |
| Source | https://community.embarcadero.com/blogs/entry/multicast-events--the-finale-38867 |

In my previous two posts I presented a technique using the new generics language feature of Delphi 2009 to create a type-safe multicast event. In the previous post, I showed how you can create a TMulticastEvent<T> instance and assign it to an event handler for an existing event on a TComponent derived type. Using the existing FreeNotification mechanism, you didn't need to worry about explicitly freeing the multicast event object. What if one of the components in the sink event handlers in the multicast event list was freed? The good thing is that the FreeNotification mechanism works both ways. We can leverage this functionality again to handle cleanup from the other direction.

In order to implement the complete cleanup for the TComponentMulticastEvent<T>, we need to know when an event handler was added and when one was removed. To do this I added these two virtual methods to the base TMulticastEvent class (the base non-generic version). There are also helper functions, RemoveInstanceReferences() and IndexOfInstance() that can be used in descendants to remove all event handlers that refer to a specific object instance and check if a specific instance is being referenced within the list.

```pas
  TMulticastEvent = class
    ...
  strict protected
    procedure EventAdded(const AMethod: TMethod); virtual;
    procedure EventRemoved(const AMethod: TMethod); virtual;
  protected
    procedure RemoveInstanceReferences(const Instance: TObject);
    function IndexOfInstance(const Instance: TObject): Integer;
    ...
  end;
```

They're not marked abstract because the immediate descendant, TMulticastEvent<T> doesn't need to and should not be forced to override them. They just do nothing in the base class. In the corresponding Add and Remove methods on TMulticastEvent, these virtual methods are then called with event just added or just removed. Now we override the EventAdded and EventRemoved methods in the TComponentMulticastEvent<T> class:

```pas
  TComponentMulticastEvent<T> = class(TMulticastEvent<T>)
    ...
  private
    FSink: TNotificationSink;
  strict protected
    procedure EventAdded(const AMethod: TMethod); override;
    procedure EventRemoved(const AMethod: TMethod); override;
    ...
  end;
```

We also need to hold a reference to the internal notification sink class in order to use its FreeNotification mechanism. Here's the implementation of these methods:

```pas
procedure TComponentMulticastEvent<T>.EventAdded(const AMethod: TMethod);
begin
  inherited;
  if TObject(AMethod.Data) is TComponent then
    FSink.FreeNotification(TComponent(AMethod.Data));
end;

procedure TComponentMulticastEvent<T>.EventRemoved(const AMethod: TMethod);
begin
  inherited;
  if (TObject(AMethod.Data) is TComponent) and (IndexOfInstance(TObject(AMethod.Data)) < 0) then
    FSink.RemoveFreeNotification(TComponent(AMethod.Data));
end;
```

And then the Notification on the private TNotificationSink class:

```pas
procedure TComponentMulticastEvent<T>.TNotificationSink.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FOwnerComp then
      Free
    else
      FEvent.RemoveInstanceReferences(AComponent);
end;
```

In the EventRemoved method we call IndexOfInstance() to ensure that there aren't multiple references to the same instance in the list before we remove the free notification hook. This is because FreeNotification will add the instance to its internal list only once.

So there you go, a multicast event that also performs full auto-cleanup for both the source and the sink instances. If the source instance goes away, the multicast event instance is automatically cleaned up. Likewise, if one of the sink event handlers' instances go away, it will automatically be removed from the list so there are no stale references. Of course, I'll remind the reader that with this implementation, it only works for TComponent derived instances. For non-TComponent derived instances, you can still use a descendant of TMulticastEvent<T> in a manner similar to TComponentMulticastEvent<T> mixed with, for instance, a technique described here.
