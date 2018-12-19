module Field = Reasonform_field;

type t('values, 'value, 'row) = {
  key: string,
  count: 'values => int,
  add: ('value, 'values) => 'values,
  push: ('value, 'values) => 'values,
  insert: (int, 'value, 'values) => 'values,
  remove: (int, 'values) => 'values,
  update: (list('value) => list('value), 'values) => 'values,
  getList: 'values => list('value),
  setList: (list('value), 'values) => 'values,
  getRow: int => 'row,
  getRows: 'values => list('row),
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

let removeIndex = (list, index) => {
  let rec aux = (list, i) =>
    switch (list, i) {
    | ([], _) => []
    | ([_, ...l], i) when i == index => l
    | ([a, ...l], i) => [a, ...aux(l, i + 1)]
    };
  if (index < 0 || index > Belt.List.length(list)) {
    failwith("invalid index");
  } else {
    aux(list, 0);
  };
};

let createField =
    (
      ~key: string,
      ~getList: 'value => list('item),
      ~setList: (list('item), 'value) => 'value,
      ~createFields: Field.wrapper('item, 'value, 'fields) => 'row,
    )
    : t('value, 'item, 'row) => {
  let getRow = index =>
    createFields({
      wrapField: field =>
        Field.wrapField(
          ~key=key ++ "[" ++ string_of_int(index) ++ "]." ++ field.key,
          ~field,
          ~getValue=user => Belt.List.getExn(getList(user), index),
          ~setValue=
            (row, value) =>
              getList(value) |> Belt.List.mapWithIndex(_, (i, e) => i == index ? row : e) |> setList(_, value),
          (),
        ),
    });
  {
    key,
    count: values => values |> getList |> Belt.List.size,
    add: (value, values) => values |> getList |> Belt.List.add(_, value) |> setList(_, values),
    push: (value, values) => values |> getList |> Belt.List.concat(_, [value]) |> setList(_, values),
    insert: (index, value, values) => values |> getList |> insert(_, index, value) |> setList(_, values),
    remove: (index, values) => values |> getList |> removeIndex(_, index) |> setList(_, values),
    update: (updater, values) => values |> getList |> updater |> setList(_, values),
    getList,
    setList,
    getRow,
    getRows: values => values |> getList |> Belt.List.mapWithIndexU(_, (. index, _) => getRow(index)),
  };
};
