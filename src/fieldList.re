type t('values, 'value, 'row) = {
  key: string,
  count: 'values => int,
  add: ('value, 'values) => 'values,
  push: ('value, 'values) => 'values,
  insert: (int, 'value, 'values) => 'values,
  getList: 'values => list('value),
  setList: (list('value), 'values) => 'values,
  getRow: int => 'row,
  bind: Bind.t('values, list('value)),
};

let insert = (list, index, elt) => {
  let rec aux = (list, i) =>
    switch (list, i) {
    | ([], i) when i == index => [elt]
    | ([], _) => []
    | ([a, ...l], i) when i == index => [elt, a, ...l]
    | ([a, ...l], i) => [a, ...aux(l, i + 1)]
    };
  if (index < 0 || index > Belt.List.length(list)) {
    failwith("invalid index");
  } else {
    aux(list, 0);
  };
};

type wrapper('item, 'value, 'fields) = {wrapField: 'a .Field.t('item, 'a) => Field.t('value, 'a)};

let createField =
    (
      ~key: string,
      ~getList: 'value => list('item),
      ~setList: (list('item), 'value) => 'value,
      ~createFields: wrapper('item, 'value, 'fields) => 'row,
    )
    : t('value, 'item, 'row) => {
  key,
  count: values => values |> getList |> Belt.List.size,
  add: (value, values) => values |> getList |> Belt.List.add(_, value) |> setList(_, values),
  push: (value, values) => values |> getList |> Belt.List.concat(_, [value]) |> setList(_, values),
  insert: (index, value, values) => values |> getList |> insert(_, index, value) |> setList(_, values),
  getList,
  setList,
  bind: Bind.bind(~key, ~getValue=getList, ~setValue=setList),
  getRow: index =>
    createFields({
      wrapField: field =>
        Field.wrapField(
          ~key=key ++ "." ++ string_of_int(index) ++ "." ++ field.key,
          ~field,
          ~getValue=user => Belt.List.getExn(getList(user), index),
          ~setValue=
            (row, value) =>
              getList(value) |> Belt.List.mapWithIndex(_, (i, e) => i == index ? row : e) |> setList(_, value),
          (),
        ),
    }),
};

let changeValues = (field, newValues, form) => Form.changeValues([field.key], newValues, form);
