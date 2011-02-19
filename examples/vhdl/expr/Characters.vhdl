library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

use work.expr_types.all;

package characters is

  subtype character_t is natural range 0 to 2**8 - 1;

  constant C_NEW_LINE        : character_t := 10;
  constant C_CARRIAGE_RETURN : character_t := 13;
  constant C_SPACE           : character_t := 32;
  constant C_DOLLAR          : character_t := 36;
  constant C_LEFT_PAREN      : character_t := 40;
  constant C_RIGHT_PAREN     : character_t := 41;
  constant C_MULT            : character_t := 42;
  constant C_PLUS            : character_t := 43;
  constant C_CLOSE_PAREN     : character_t := 41;
  constant C_lower_n         : character_t := 110;

  function c_pos ( char : in character )
    return character_t;

end package characters;

package body characters is

  function c_pos ( char : in character )
    return character_t is
  begin

-- return conv_integer (character'pos (char));

    case char is

      when LF  => return C_NEW_LINE;
      when VT  => return C_CARRIAGE_RETURN;
      when ' ' => return C_SPACE;
      when '$' => return C_DOLLAR;
      when '(' => return C_LEFT_PAREN;
      when ')' => return C_RIGHT_PAREN;
      when '*' => return C_MULT;
      when '+' => return C_PLUS;
      when 'n' => return C_lower_n;

      when others => return C_DOLLAR;

    end case;

  end c_pos;

end package body characters;
