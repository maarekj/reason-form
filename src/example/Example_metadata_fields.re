module Value = {
  type t = {
    title: string,
    desc: string,
  };

  let empty: t = {title: "", desc: ""};
};

let title =
  Field.createField(
    ~key="title",
    ~getValue=values => values.Value.title,
    ~setValue=(value, values) => {...values, title: value},
  );

let desc =
  Field.createField(
    ~key="desc",
    ~getValue=values => values.Value.desc,
    ~setValue=(value, values) => {...values, desc: value},
  );

type fields('values) = {
  title: Field.t('values, string),
  desc: Field.t('values, string),
};

module Fields = (T: {type t;}) => {
  type t = fields(T.t);
};

let wrapFields = ({Field.wrapField}) => {title: wrapField(title), desc: wrapField(desc)};
let wrapOptionFields = ({Field.wrapField}) => {
  title: wrapField(Field.wrapOptionField(~field=title, ~empty=Value.empty, ())),
  desc: wrapField(Field.wrapOptionField(~field=desc, ~empty=Value.empty, ())),
};

let validate = (metadata: Value.t, field: fields(_), form) => {
  let addError = Helper.addError;
  let id = form => form;
  form
  |> (
    switch (metadata.title) {
    | "" => addError(`field(field.title), "Title is required.")
    | "forbidden" => addError(`field(field.title), "Forbidden title.")
    | _ => id
    }
  )
  |> (
    switch (metadata.desc) {
    | "forbidden" => addError(`field(field.desc), "Forbidden desc.")
    | _ => id
    }
  );
};
