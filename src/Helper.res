let getValue = event => (event->ReactEvent.Form.target)["value"]

let changeFieldValue = (field: Field.t<_, _>, value, form) => {
  let values = Form.getValues(form)
  let newValues = field.setValue(value, values)
  Form.changeValues(list{field.key}, newValues, form)
}

let update = (field: Field.t<_, _>, updater, form) => {
  let values = field.getValue(Form.getValues(form))
  changeFieldValue(field, updater(values), form)
}

module List = {
  open Belt

  %%private(
    let insert = (list, index, elt) => {
      let rec aux = (list, i) =>
        switch (list, i) {
        | (list{}, i) if i == index => list{elt}
        | (list{}, _) => list{}
        | (list{a, ...l}, i) if i == index => list{elt, a, ...l}
        | (list{a, ...l}, i) => list{a, ...aux(l, i + 1)}
        }

      if index < 0 || index > List.length(list) {
        list
      } else {
        aux(list, 0)
      }
    }
  )

  %%private(
    let removeIndex = (list, index) => {
      let rec aux = (list, i) =>
        switch (list, i) {
        | (list{}, _) => list{}
        | (list{_, ...l}, i) if i == index => l
        | (list{a, ...l}, i) => list{a, ...aux(l, i + 1)}
        }

      if index < 0 || index > List.length(list) {
        list
      } else {
        aux(list, 0)
      }
    }
  )

  let add = (field: Field.t<_, _>, value, form) => update(field, List.add(_, value), form)

  let push = (field: Field.t<_, _>, value, form) =>
    update(field, list => list->List.concat(list{value}), form)

  let insert = (field: Field.t<_, _>, index, value, form) =>
    update(field, insert(_, index, value), form)

  let remove = (field: Field.t<_, _>, index, form) => update(field, removeIndex(_, index), form)
}

module StringMap = {
  module StringMap = Belt.Map.String

  let set = (field: Field.t<_, _>, key, value, form) =>
    update(field, StringMap.set(_, key, value), form)

  let remove = (field: Field.t<_, _>, key, form) => update(field, StringMap.remove(_, key), form)
}
