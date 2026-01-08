module type T =
  sig
  end

module type Config =
  sig
  end

module Make (Config : Config) : T =
  struct
  end

