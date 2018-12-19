open ReasonForm;

module Value = {
  type t = {
    street: string,
    city: string,
  };

  let empty: t = {street: "", city: ""};
};

let street =
  Field.createField(
    ~key="street",
    ~getValue=values => values.Value.street,
    ~setValue=(value, values) => {...values, street: value},
  );
let city =
  Field.createField(
    ~key="city",
    ~getValue=values => values.Value.city,
    ~setValue=(value, values) => {...values, city: value},
  );

type fields('values) = {
  street: Field.t('values, string),
  city: Field.t('values, string),
};

module Fields = (T: {type t;}) => {
  type t = fields(T.t);
};

let wrapFields = ({Field.wrapField}) => {street: wrapField(street), city: wrapField(city)};
let wrapOptionFields = ({Field.wrapField}) => {
  street: wrapField(Field.wrapOptionField(~field=street, ~empty=Value.empty, ())),
  city: wrapField(Field.wrapOptionField(~field=city, ~empty=Value.empty, ())),
};

let validate = (address: Value.t, field: fields(_), form) => {
  let addError = Helper.addError;
  let id = form => form;
  form
  |> (
    switch (address.street) {
    | "" => addError(`field(field.street), "Street is required.")
    | "forbidden" => addError(`field(field.street), "Forbidden street.")
    | _ => id
    }
  )
  |> (
    switch (address.city) {
    | "" => addError(`field(field.city), "City is required.")
    | "forbidden" => addError(`field(field.city), "Forbidden city.")
    | _ => id
    }
  );
};
