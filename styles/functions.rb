ONE = Sass::Script::Value::Number.new(1)
module Sass::Script::Functions
  def pow(base, exponent)
    assert_type base, :Number
    assert_type exponent, :Number

    exponent = exponent.to_i
    if exponent < 0
      exponent = - exponent
      base = ONE.div(base)
    end

    result = ONE
    exponent.times do
      result = result.times base
    end
    result
  end
end
