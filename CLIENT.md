View(elTpl: string)
insertAfter(el: HTMLElement)
insertBefore(el: HTMLElement)
appendTo(el: HTMLElement)
prependTo(el: HTMLElement)
detach()

TemplateView(elTpl: string, template: any => string)
render(data: any)

ObservableView(elTpl: string, template: any => string)
setData(data: Observable)

ObservableListView(elTpl: string, template: any => string)
setData(data: ObservableList)

ViewCollection(elTpl: string, childTpl: string?)
add(view: View)
remove(view: View)

SlotViewCollection(elTpl: string, childTpl: string?)
set(slot: string; view: View)
remove(slot: string)

Observable.changed: Signal<{attr:string; value:any}>
ObservableList.inserted: Signal<{index:number; value:any}>
ObservableList.removed: Signal<{index:number; value:any}>


* Constructor must render View. Parent views must not depend on state
  of children.

DOM Abstraction
---------------

We have to provide to abstractions of the DOM of a view: Events and
access to DOM elements. The abstraction must handle changes to the DOM
(such as rerendering the complete content) transparently.

This, as well as methods that cache part of them DOM, requires a
`domChanged` signal that is triggered each time part of the dom is
replaced. In particular this applies to the various `render()` methods.

### Events

Events should be declared in a static manner—assigning each DOM event
a signal that it triggers.

### Access to DOM Elements

We provide a JQuery-like access to elements with the `$()` method.
