module Value = {
  type t = {
    street: string,
    city: string,
    zipcode: option(string),
  };

  let empty: t = {street: "", city: "", zipcode: None};
};

type fields('t, 'self) = {
  self: Field.t('t, 'self),
  street: Field.t('t, string),
  city: Field.t('t, string),
  zipcode: Field.t('t, option(string)),
};

let createFields = (self, baseField) =>
  Field.{
    self,
    street:
      baseField
      +|> createField(~key="street", ~getValue=(v: Value.t) => v.street, ~setValue=(street, v) => {...v, street}),
    city:
      baseField +|> createField(~key="city", ~getValue=(v: Value.t) => v.city, ~setValue=(city, v) => {...v, city}),
    zipcode:
      baseField
      +|> createField(
            ~key="numero",
            ~getValue=(v: Value.t) => v.zipcode,
            ~setValue=(zipcode, v) => {...v, zipcode},
          ),
  };

let validateValue = (address: Value.t, fields: fields(_, _), form) => {
  let addError = f => Form.addError(f.Field.key);
  let id = form => form;
  form
  |> (
    switch (address.street) {
    | "" => addError(fields.street, "Street is required.")
    | "forbidden" => addError(fields.street, "Forbidden street.")
    | _ => id
    }
  )
  |> (
    switch (address.city) {
    | "" => addError(fields.city, "City is required.")
    | "forbidden" => addError(fields.city, "Forbidden city.")
    | _ => id
    }
  );
};

let validate = (fields: fields(_, Value.t), form) => {
  let address = fields.self.getValue(Form.getValues(form));
  validateValue(address, fields, form);
};

let validateOptional = (fields: fields(_, option(Value.t)), form) => {
  switch (fields.self.getValue(Form.getValues(form))) {
  | None => form
  | Some(address) => validateValue(address, fields, form)
  };
};
