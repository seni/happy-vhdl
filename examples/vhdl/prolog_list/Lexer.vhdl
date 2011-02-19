library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

use work.prolog_list_types.all;
use work.Characters.all;

entity lexer is
  port ( char               : in  character_t;
         terminal           : out terminal_t;
         next_character_req : out std_logic;
         next_terminal_ack  : in  std_logic;
         next_terminal_rdy  : out std_logic;
         lexer_fail         : out std_logic;
         clock, reset       : in  std_logic
  );
end lexer;

architecture lexer_architecture of lexer is

  type state_t is ( start, end_of_file, fail,
                    send_terminal, -- send_terminal2,
                    open_list,
                    close_list,
                    comma,
                    ht_sep,
                    digit0,
                    digit19,
                    integer_constant,
                    hexadecimal_constant,
                    hexadecimal_constant_indicator,
                    variable_indicator_char,
                    capital,
                    named_variable
  );

  -- signals
  signal s_current_state, s_next_state       : state_t;
  signal s_current_terminal, s_next_terminal : terminal_t;
  signal s_next_terminal_ack                 : std_logic;

--  signal char_num                            : character_t;

  signal alpha_numeric                       : boolean;
  signal capital_letter                      : boolean;

  impure function new_terminal return state_t is

    variable ret : state_t := fail;

  begin

    if (char = C_SPACE) or
      (char = C_NEW_LINE) or
      (char = C_CARRIAGE_RETURN) then -- eat delimiters
      ret := start;
    end if;

    if char = C_OPEN_LIST then
      ret := open_list;
    end if;

    if char = C_CLOSE_LIST then
      ret := close_list;
    end if;

    if char = C_COMMA then
      ret := comma;
    end if;

    if char = C_HT_SEP then
      ret := ht_sep;
    end if;

    if (char = C_0)
    then
      ret := digit0;
    end if;

    if (char >= C_1) and
      (char <= C_9)
    then
      ret := digit19;
    end if;

    if char = C_UNDERSCORE then
      ret := variable_indicator_char;
    end if;

    if capital_letter then
      ret := capital;
    end if;

    if char = C_DOLLAR then
      ret := end_of_file;
    end if;

    return ret;

  end new_terminal;

begin

--  char_num <= integer_conv (character'pos (char));

  alpha_numeric <=
    ((char >= C_0) and (char <= C_UPPER_Z)) or
    ((char >= c_lower_a) and (char <= c_lower_z));

  capital_letter <=
    (char >= C_UPPER_A) and
    (char <= C_UPPER_Z);

  terminal <= s_current_terminal;

  lexer : process ( s_current_state, char,
                    s_next_terminal_ack,
                    s_current_terminal,
                    alpha_numeric
                  )
  begin

    next_character_req <= '1';
    next_terminal_rdy <= '0';
    lexer_fail <= '0';
    s_next_state <= fail;
    s_next_terminal <= s_current_terminal;

    case s_current_state is
      when start =>
        s_next_state <= new_terminal;

      when open_list =>
        s_next_terminal <= tToken_open_list;
        s_next_state    <= send_terminal;

      when close_list =>
        s_next_terminal <= tToken_close_list;
        s_next_state    <= send_terminal;

      when comma =>
        s_next_terminal <= tToken_comma;
        s_next_state    <= send_terminal;

      when ht_sep =>
        s_next_terminal <= tToken_ht_sep;
        s_next_state    <= send_terminal;

      when digit0 =>
        if (char = C_UPPER_X) or
           (char = c_lower_x)
        then
          s_next_state <= hexadecimal_constant_indicator;
        elsif (char >= C_0 and
               char <= C_9)
        then
          s_next_state <= integer_constant;
        else
          s_next_terminal <= tToken_Integer_constant;
          s_next_state    <= send_terminal;
        end if;

      when digit19 =>
        if (char >= C_0 and
            char <= C_9)
        then
          s_next_state <= integer_constant;
        else
          s_next_terminal <= tToken_Integer_constant;
          s_next_state    <= send_terminal;
        end if;

      when integer_constant =>
        if (char >= C_0 and
            char <= C_9)
        then
          s_next_state <= integer_constant;
        else
          s_next_terminal <= tToken_Integer_constant;
          s_next_state    <= send_terminal;
        end if;

      when hexadecimal_constant_indicator =>
        if (char >= C_0 and
            char <= C_9)
        then
          s_next_state <= hexadecimal_constant;
        else
          s_next_state <= fail;
        end if;

      when hexadecimal_constant =>
        if (char >= C_0 and
            char <= C_9)
        then
          s_next_state <= hexadecimal_constant;
        else
          s_next_terminal <= tToken_Hexadecimal_constant;
          s_next_state    <= send_terminal;
        end if;

      when variable_indicator_char =>
        if alpha_numeric then
          s_next_state <= named_variable;
        else
          s_next_terminal <= tToken_Anonymous_Variable;
          s_next_state    <= send_terminal;
        end if;

      when capital =>
        if alpha_numeric then
          s_next_state <= named_variable;
        else
          s_next_terminal <= tToken_Named_Variable;
          s_next_state    <= send_terminal;
        end if;

      when named_variable =>
        if alpha_numeric then
          s_next_state <= named_variable;
        else
          s_next_terminal <= tToken_Named_Variable;
          s_next_state    <= send_terminal;
        end if;

      when end_of_file =>
        s_next_terminal <= eof;
        s_next_state    <= send_terminal;

      when fail =>
        next_character_req <= '0';
        lexer_fail   <= '1';
        s_next_state <= fail;

      when send_terminal =>
        next_character_req <= '0';
        if s_next_terminal_ack = '0' then
          next_terminal_rdy  <= '1';
          s_next_state       <= send_terminal;
        else
--           s_next_state <= send_terminal2;
          s_next_state <= new_terminal;
        end if;

--       when send_terminal2 =>
--         next_terminal_rdy  <= '0';
--         next_character_req <= '0';
--         if s_next_terminal_ack = '1' then
--           s_next_state <= send_terminal2;
--         else
--           next_character_req <= '0';
--           s_next_state <= new_terminal;
--         end if;

      when others =>
        next_character_req <= '0';
        s_next_state <= fail;

    end case;

  end process lexer;

  next_state_update: process ( clock, reset )
  begin
    if (reset ='1') then
      s_current_state         <= start;
      s_current_terminal      <= eof;
      s_next_terminal_ack     <= '0';
    elsif rising_edge (clock) then
      s_current_state         <= s_next_state;
      s_current_terminal      <= s_next_terminal;
      s_next_terminal_ack     <= next_terminal_ack;
    end if;
  end process next_state_update;

end lexer_architecture;
