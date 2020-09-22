let hello = () =>
  Pastel.(
    <Pastel>
      <Pastel color=Red> "Hello" </Pastel>
      ", "
      <Pastel color=Green> "World" </Pastel>
      "!"
    </Pastel>
  );

let logKeyValue = (key, value) => 
  Pastel.(
    <Pastel>
      <Pastel color=Red> (key ++ ": ") </Pastel>
      <Pastel color=Green> value </Pastel>
    </Pastel>
  );

let logListString = (options, title) =>
  Pastel.(
    <Pastel>
      <Pastel color=Red> (title ++ ": ") </Pastel>
      <Pastel color=Green> (Option.get(options) |> String.concat(",")) </Pastel>
    </Pastel>
  );