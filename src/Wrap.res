type listener<'content> = 'content => unit

let globalId = ref(0)
let incrGlobalId = () => {
  incr(globalId)
  globalId.contents
}

type t<'content> = {
  id: string,
  initial: 'content,
  mutable content: 'content,
  mutable listeners: list<listener<'content>>,
}

let make = (~id=?, initialContent) => {
  id: switch id {
  | Some(id) => id
  | None => "form-" ++ string_of_int(incrGlobalId())
  },
  initial: initialContent,
  content: initialContent,
  listeners: list{},
}

let removeListener = (wrap, listener) =>
  wrap.listeners = wrap.listeners->Belt.List.keep(a => listener !== a)
let addListener = (wrap, listener) => {
  wrap.listeners = list{listener, ...wrap.listeners}
  () => removeListener(wrap, listener)
}
let content = wrap => wrap.content
let id = wrap => wrap.id

let dispatch = (wrap, action) => {
  wrap.content = action(wrap.content)
  wrap.listeners->Belt.List.forEach(listener => listener(wrap.content))
}
