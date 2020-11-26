--------------------------------------------------------------------------------
--                  LZOCShifter_2_to_2_counting_4_F250_uid29
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: I OZb
-- Output signals: Count O

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity LZOCShifter_2_to_2_counting_4_F250_uid29 is
    port (clk : in std_logic;
          I : in  std_logic_vector(1 downto 0);
          OZb : in  std_logic;
          Count : out  std_logic_vector(1 downto 0);
          O : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of LZOCShifter_2_to_2_counting_4_F250_uid29 is
signal level2 :  std_logic_vector(1 downto 0);
signal sozb :  std_logic;
signal count1 :  std_logic;
signal level1 :  std_logic_vector(1 downto 0);
signal count0 :  std_logic;
signal level0 :  std_logic_vector(1 downto 0);
signal sCount :  std_logic_vector(1 downto 0);
begin
   level2 <= I ;
   sozb<= OZb;
   count1<= '1' when level2(1 downto 0) = (1 downto 0=>sozb) else '0';
   level1<= level2(1 downto 0) when count1='0' else (1 downto 0 => '0');

   count0<= '1' when level1(1 downto 1) = (1 downto 1=>sozb) else '0';
   level0<= level1(1 downto 0) when count0='0' else level1(0 downto 0) & (0 downto 0 => '0');

   O <= level0;
   sCount <= count1 & count0;
   Count <= sCount;
end architecture;

--------------------------------------------------------------------------------
--                                  Posit2PD
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: BSC / UPC - Ledoux Louis
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: posit_i
-- Output signals: pd_o

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Posit2PD is
    port (clk : in std_logic;
          posit_i : in  std_logic_vector(3 downto 0);
          pd_o : out  std_logic_vector(6 downto 0)   );
end entity;

architecture arch of Posit2PD is
   component LZOCShifter_2_to_2_counting_4_F250_uid29 is
      port ( clk : in std_logic;
             I : in  std_logic_vector(1 downto 0);
             OZb : in  std_logic;
             Count : out  std_logic_vector(1 downto 0);
             O : out  std_logic_vector(1 downto 0)   );
   end component;

signal sign :  std_logic;
signal regime_check :  std_logic;
signal remainder :  std_logic_vector(1 downto 0);
signal not_s :  std_logic;
signal zero_NAR :  std_logic;
signal is_NAR :  std_logic;
signal is_zero :  std_logic;
signal neg_count :  std_logic;
signal lzCount :  std_logic_vector(1 downto 0);
signal usefulBits :  std_logic_vector(1 downto 0);
signal extended_neg_count :  std_logic_vector(2 downto 0);
signal comp2_range_count :  std_logic_vector(2 downto 0);
signal fraction :  std_logic_vector(0 downto 0);
signal exponent :  std_logic_vector(2 downto 0);
signal biased_exponent :  std_logic_vector(2 downto 0);
begin
sign <= posit_i(3);
regime_check <= posit_i(2);
remainder <= posit_i(1 downto 0);
not_s <= not sign;
zero_NAR <= not regime_check when remainder="00" else '0';
is_NAR<= zero_NAR and sign;
is_zero<= zero_NAR and not_s;
neg_count<= not (sign xor regime_check);
   lzoc: LZOCShifter_2_to_2_counting_4_F250_uid29
      port map ( clk  => clk,
                 I => remainder,
                 OZb => regime_check,
                 Count => lzCount,
                 O => usefulBits);
with neg_count  select  extended_neg_count <= 
   "111" when '1', 
   "000" when others;
comp2_range_count<= extended_neg_count xor ("0" & lzCount);
fraction<= usefulBits(0 downto 0);
exponent <= comp2_range_count;
biased_exponent<= exponent + 2;
pd_o <= sign & is_NAR & is_zero & biased_exponent & fraction;
end architecture;

--------------------------------------------------------------------------------
--              LZOCShifterSticky_12_to_3_counting_16_F250_uid33
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: I OZb
-- Output signals: Count O Sticky

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity LZOCShifterSticky_12_to_3_counting_16_F250_uid33 is
    port (clk : in std_logic;
          I : in  std_logic_vector(11 downto 0);
          OZb : in  std_logic;
          Count : out  std_logic_vector(3 downto 0);
          O : out  std_logic_vector(2 downto 0);
          Sticky : out  std_logic   );
end entity;

architecture arch of LZOCShifterSticky_12_to_3_counting_16_F250_uid33 is
signal level4 :  std_logic_vector(11 downto 0);
signal sozb :  std_logic;
signal sticky4 :  std_logic;
signal count3 :  std_logic;
signal level3 :  std_logic_vector(11 downto 0);
signal sticky_high_3 :  std_logic;
signal sticky_low_3 :  std_logic;
signal sticky3 :  std_logic;
signal count2 :  std_logic;
signal level2 :  std_logic_vector(6 downto 0);
signal sticky_high_2 :  std_logic;
signal sticky_low_2 :  std_logic;
signal sticky2 :  std_logic;
signal count1 :  std_logic;
signal level1 :  std_logic_vector(3 downto 0);
signal sticky_high_1 :  std_logic;
signal sticky_low_1 :  std_logic;
signal sticky1 :  std_logic;
signal count0 :  std_logic;
signal level0 :  std_logic_vector(2 downto 0);
signal sticky_high_0 :  std_logic;
signal sticky_low_0 :  std_logic;
signal sticky0 :  std_logic;
signal sCount :  std_logic_vector(3 downto 0);
begin
   level4 <= I ;
   sozb<= OZb;
   sticky4 <= '0' ;
   count3<= '1' when level4(11 downto 4) = (11 downto 4=>sozb) else '0';
   level3<= level4(11 downto 0) when count3='0' else level4(3 downto 0) & (7 downto 0 => '0');
   sticky_high_3<= '0';
   sticky_low_3<= '0';
   sticky3<= sticky4 or sticky_high_3 when count3='0' else sticky4 or sticky_low_3;

   count2<= '1' when level3(11 downto 8) = (11 downto 8=>sozb) else '0';
   level2<= level3(11 downto 5) when count2='0' else level3(7 downto 1);
   sticky_high_2<= '0'when level3(4 downto 0) = CONV_STD_LOGIC_VECTOR(0,5) else '1';
   sticky_low_2<= '0'when level3(0 downto 0) = CONV_STD_LOGIC_VECTOR(0,1) else '1';
   sticky2<= sticky3 or sticky_high_2 when count2='0' else sticky3 or sticky_low_2;

   count1<= '1' when level2(6 downto 5) = (6 downto 5=>sozb) else '0';
   level1<= level2(6 downto 3) when count1='0' else level2(4 downto 1);
   sticky_high_1<= '0'when level2(2 downto 0) = CONV_STD_LOGIC_VECTOR(0,3) else '1';
   sticky_low_1<= '0'when level2(0 downto 0) = CONV_STD_LOGIC_VECTOR(0,1) else '1';
   sticky1<= sticky2 or sticky_high_1 when count1='0' else sticky2 or sticky_low_1;

   count0<= '1' when level1(3 downto 3) = (3 downto 3=>sozb) else '0';
   level0<= level1(3 downto 1) when count0='0' else level1(2 downto 0);
   sticky_high_0<= '0'when level1(0 downto 0) = CONV_STD_LOGIC_VECTOR(0,1) else '1';
   sticky_low_0<= '0';
   sticky0<= sticky1 or sticky_high_0 when count0='0' else sticky1 or sticky_low_0;

   O <= level0;
   sCount <= count3 & count2 & count1 & count0;
   Count <= sCount;
   Sticky <= sticky0;
end architecture;

--------------------------------------------------------------------------------
--                  RightShifterSticky4_by_max_4_F250_uid35
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca (2008-2011), Florent de Dinechin (2008-2019)
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: X S padBit
-- Output signals: R Sticky

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity RightShifterSticky4_by_max_4_F250_uid35 is
    port (clk : in std_logic;
          X : in  std_logic_vector(3 downto 0);
          S : in  std_logic_vector(2 downto 0);
          padBit : in  std_logic;
          R : out  std_logic_vector(3 downto 0);
          Sticky : out  std_logic   );
end entity;

architecture arch of RightShifterSticky4_by_max_4_F250_uid35 is
signal ps, ps_d1 :  std_logic_vector(2 downto 0);
signal level3 :  std_logic_vector(3 downto 0);
signal stk2 :  std_logic;
signal level2 :  std_logic_vector(3 downto 0);
signal stk1, stk1_d1 :  std_logic;
signal level1, level1_d1 :  std_logic_vector(3 downto 0);
signal stk0 :  std_logic;
signal level0 :  std_logic_vector(3 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            ps_d1 <=  ps;
            stk1_d1 <=  stk1;
            level1_d1 <=  level1;
         end if;
      end process;
   ps<= S;
   level3<= X;
   stk2 <= '1' when (level3(3 downto 0)/="0000" and ps(2)='1')   else '0';
   level2 <=  level3 when  ps(2)='0'    else (3 downto 0 => padBit) ;
   stk1 <= '1' when (level2(1 downto 0)/="00" and ps(1)='1') or stk2 ='1'   else '0';
   level1 <=  level2 when  ps(1)='0'    else (1 downto 0 => padBit) & level2(3 downto 2);
   stk0 <= '1' when (level1_d1(0 downto 0)/="0" and ps_d1(0)='1') or stk1_d1 ='1'   else '0';
   level0 <=  level1 when  ps(0)='0'    else (0 downto 0 => padBit) & level1(3 downto 1);
   R <= level0;
   Sticky <= stk0;
end architecture;

--------------------------------------------------------------------------------
--                                Quire2Posit
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Ledoux Louis - BSC / UPC
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: A
-- Output signals: posit_O

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Quire2Posit is
    port (clk : in std_logic;
          A : in  std_logic_vector(11 downto 0);
          posit_O : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of Quire2Posit is
   component LZOCShifterSticky_12_to_3_counting_16_F250_uid33 is
      port ( clk : in std_logic;
             I : in  std_logic_vector(11 downto 0);
             OZb : in  std_logic;
             Count : out  std_logic_vector(3 downto 0);
             O : out  std_logic_vector(2 downto 0);
             Sticky : out  std_logic   );
   end component;

   component RightShifterSticky4_by_max_4_F250_uid35 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(3 downto 0);
             S : in  std_logic_vector(2 downto 0);
             padBit : in  std_logic;
             R : out  std_logic_vector(3 downto 0);
             Sticky : out  std_logic   );
   end component;

signal quire :  std_logic_vector(11 downto 0);
signal count_bit, count_bit_d1 :  std_logic;
signal count_lzoc_o :  std_logic_vector(3 downto 0);
signal frac_lzoc_o :  std_logic_vector(2 downto 0);
signal sticky_lzoc_o, sticky_lzoc_o_d1 :  std_logic;
signal biased_exp :  std_logic_vector(3 downto 0);
signal unbiased_exp :  std_logic_vector(3 downto 0);
signal fraction, fraction_d1 :  std_logic_vector(1 downto 0);
signal bin_regime :  std_logic_vector(2 downto 0);
signal first_regime :  std_logic;
signal regime :  std_logic_vector(2 downto 0);
signal pad :  std_logic;
signal start_regime :  std_logic_vector(1 downto 0);
signal in_shift :  std_logic_vector(3 downto 0);
signal extended_posit :  std_logic_vector(3 downto 0);
signal pre_sticky :  std_logic;
signal truncated_posit, truncated_posit_d1 :  std_logic_vector(2 downto 0);
signal lsb, lsb_d1 :  std_logic;
signal guard, guard_d1 :  std_logic;
signal sticky :  std_logic;
signal round_bit :  std_logic;
signal rounded_reg_exp_frac :  std_logic_vector(2 downto 0);
signal rounded_posit :  std_logic_vector(3 downto 0);
signal is_zero, is_zero_d1 :  std_logic;
signal rounded_posit_zero :  std_logic_vector(3 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            count_bit_d1 <=  count_bit;
            sticky_lzoc_o_d1 <=  sticky_lzoc_o;
            fraction_d1 <=  fraction;
            truncated_posit_d1 <=  truncated_posit;
            lsb_d1 <=  lsb;
            guard_d1 <=  guard;
            is_zero_d1 <=  is_zero;
         end if;
      end process;
   quire <= A;
   count_bit <= quire(11);
   lzoc_inst: LZOCShifterSticky_12_to_3_counting_16_F250_uid33
      port map ( clk  => clk,
                 I => quire,
                 OZb => count_bit,
                 Count => count_lzoc_o,
                 O => frac_lzoc_o,
                 Sticky => sticky_lzoc_o);

   biased_exp <= CONV_STD_LOGIC_VECTOR(11,4) - count_lzoc_o;
   unbiased_exp <= biased_exp - CONV_STD_LOGIC_VECTOR(4,4);

   fraction <= frac_lzoc_o (1 downto 0);
bin_regime<= unbiased_exp(2 downto 0);
first_regime<= unbiased_exp(3);
with first_regime  select  regime <= 
   bin_regime when '0', 
   not bin_regime when others;
pad<= not(first_regime xor count_bit);
with pad  select  start_regime <= 
   "01" when '0', 
   "10" when others; 
in_shift <= start_regime & fraction;
   rshift: RightShifterSticky4_by_max_4_F250_uid35
      port map ( clk  => clk,
                 S => regime,
                 X => in_shift,
                 padBit => pad,
                 R => extended_posit,
                 Sticky => pre_sticky);
truncated_posit<= extended_posit(3 downto 1);
lsb <= extended_posit(1);
guard <= extended_posit(0);
sticky <= fraction_d1(0) or pre_sticky or sticky_lzoc_o_d1;
round_bit<= guard_d1 and (sticky or lsb_d1);
rounded_reg_exp_frac<= truncated_posit_d1 + round_bit;
rounded_posit <= count_bit_d1 & rounded_reg_exp_frac;
is_zero <= count_lzoc_o(3) when fraction="00" else '0';
rounded_posit_zero<= rounded_posit when is_zero_d1= '0' else "0000";
posit_O <= rounded_posit_zero;
end architecture;

--------------------------------------------------------------------------------
--                          DSPBlock_2x2_F250_uid57
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity DSPBlock_2x2_F250_uid57 is
    port (clk : in std_logic;
          X : in  std_logic_vector(1 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of DSPBlock_2x2_F250_uid57 is
signal Mint :  std_logic_vector(3 downto 0);
signal M :  std_logic_vector(3 downto 0);
signal Rtmp :  std_logic_vector(3 downto 0);
begin
   Mint <= std_logic_vector(unsigned(X) * unsigned(Y)); -- multiplier
   M <= Mint(3 downto 0);
   Rtmp <= M;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                          IntMultiplier_F250_uid53
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Martin Kumm, Florent de Dinechin, Kinga Illyes, Bogdan Popa, Bogdan Pasca, 2012
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity IntMultiplier_F250_uid53 is
    port (clk : in std_logic;
          X : in  std_logic_vector(1 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of IntMultiplier_F250_uid53 is
   component DSPBlock_2x2_F250_uid57 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(1 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(3 downto 0)   );
   end component;

signal XX_m54 :  std_logic_vector(1 downto 0);
signal YY_m54 :  std_logic_vector(1 downto 0);
signal tile_0_X :  std_logic_vector(1 downto 0);
signal tile_0_Y :  std_logic_vector(1 downto 0);
signal tile_0_output :  std_logic_vector(3 downto 0);
signal tile_0_filtered_output :  std_logic_vector(3 downto 0);
signal bh55_w0_0 :  std_logic;
signal bh55_w1_0 :  std_logic;
signal bh55_w2_0 :  std_logic;
signal bh55_w3_0 :  std_logic;
signal tmp_bitheapResult_bh55_3 :  std_logic_vector(3 downto 0);
signal bitheapResult_bh55 :  std_logic_vector(3 downto 0);
begin
   XX_m54 <= X ;
   YY_m54 <= Y ;
   tile_0_X <= X(1 downto 0);
   tile_0_Y <= Y(1 downto 0);
   tile_0_mult: DSPBlock_2x2_F250_uid57
      port map ( clk  => clk,
                 X => tile_0_X,
                 Y => tile_0_Y,
                 R => tile_0_output);

tile_0_filtered_output <= tile_0_output(3 downto 0);
   bh55_w0_0 <= tile_0_filtered_output(0);
   bh55_w1_0 <= tile_0_filtered_output(1);
   bh55_w2_0 <= tile_0_filtered_output(2);
   bh55_w3_0 <= tile_0_filtered_output(3);

   -- Adding the constant bits
      -- All the constant bits are zero, nothing to add

   tmp_bitheapResult_bh55_3 <= bh55_w3_0 & bh55_w2_0 & bh55_w1_0 & bh55_w0_0;
   bitheapResult_bh55 <= tmp_bitheapResult_bh55_3;
   R <= bitheapResult_bh55(3 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                      LeftShifter4_by_max_8_F250_uid60
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca (2008-2011), Florent de Dinechin (2008-2019)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: X S
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity LeftShifter4_by_max_8_F250_uid60 is
    port (clk : in std_logic;
          X : in  std_logic_vector(3 downto 0);
          S : in  std_logic_vector(3 downto 0);
          R : out  std_logic_vector(11 downto 0)   );
end entity;

architecture arch of LeftShifter4_by_max_8_F250_uid60 is
signal ps :  std_logic_vector(3 downto 0);
signal level0 :  std_logic_vector(3 downto 0);
signal level1 :  std_logic_vector(4 downto 0);
signal level2 :  std_logic_vector(6 downto 0);
signal level3 :  std_logic_vector(10 downto 0);
signal level4 :  std_logic_vector(18 downto 0);
begin
   ps<= S;
   level0<= X;
   level1<= level0 & (0 downto 0 => '0') when ps(0)= '1' else     (0 downto 0 => '0') & level0;
   R <= level4(11 downto 0);
   level2<= level1 & (1 downto 0 => '0') when ps(1)= '1' else     (1 downto 0 => '0') & level1;
   R <= level4(11 downto 0);
   level3<= level2 & (3 downto 0 => '0') when ps(2)= '1' else     (3 downto 0 => '0') & level2;
   R <= level4(11 downto 0);
   level4<= level3 & (7 downto 0 => '0') when ps(3)= '1' else     (7 downto 0 => '0') & level3;
   R <= level4(11 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                            pdfdp_4_0_F250_uid51
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Ledoux Louis - BSC / UPC
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: pd_x pd_y FTZ EOB
-- Output signals: A EOB_Q

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity pdfdp_4_0_F250_uid51 is
    port (clk, rst : in std_logic;
          pd_x : in  std_logic_vector(6 downto 0);
          pd_y : in  std_logic_vector(6 downto 0);
          FTZ : in  std_logic;
          EOB : in  std_logic;
          A : out  std_logic_vector(11 downto 0);
          EOB_Q : out  std_logic   );
end entity;

architecture arch of pdfdp_4_0_F250_uid51 is
   component IntMultiplier_F250_uid53 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(1 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(3 downto 0)   );
   end component;

   component LeftShifter4_by_max_8_F250_uid60 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(3 downto 0);
             S : in  std_logic_vector(3 downto 0);
             R : out  std_logic_vector(11 downto 0)   );
   end component;

signal sign_X :  std_logic;
signal sign_Y :  std_logic;
signal sign_M :  std_logic;
signal is_not_zero_X :  std_logic;
signal is_not_zero_Y :  std_logic;
signal mantissa_X :  std_logic_vector(1 downto 0);
signal mantissa_Y :  std_logic_vector(1 downto 0);
signal significand_product :  std_logic_vector(3 downto 0);
signal scale_X_biased :  std_logic_vector(2 downto 0);
signal scale_Y_biased :  std_logic_vector(2 downto 0);
signal product_scale_twice_biased :  std_logic_vector(3 downto 0);
signal shift_value :  std_logic_vector(3 downto 0);
signal shift_value_S_bus :  std_logic_vector(3 downto 0);
signal shifted_frac :  std_logic_vector(11 downto 0);
signal summand :  std_logic_vector(8 downto 0);
signal summand1c :  std_logic_vector(8 downto 0);
signal ext_summand1c :  std_logic_vector(11 downto 0);
signal not_ftz :  std_logic;
signal EOB_internal, EOB_internal_d1 :  std_logic;
signal not_ftz_sync :  std_logic;
signal carry_0_sync :  std_logic;
signal EOB_internal_delayed :  std_logic;
signal carry_0 :  std_logic;
signal summand_0 :  std_logic_vector(11 downto 0);
signal summand_and_carry_0 :  std_logic_vector(12 downto 0);
signal acc_0, acc_0_d1 :  std_logic_vector(12 downto 0);
signal acc_0_q :  std_logic_vector(12 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            EOB_internal_d1 <=  EOB_internal;
         end if;
      end process;
   process(clk, rst)
      begin
         if rst = '1' then
            acc_0_d1 <=  (others => '0');
         elsif clk'event and clk = '1' then
            acc_0_d1 <=  acc_0;
         end if;
      end process;
------------------------------- sign processing -------------------------------
   sign_X <= pd_x(6);
   sign_Y <= pd_y(6);
   sign_M <= sign_X xor sign_Y;

------------------------------- zero processing -------------------------------
   is_not_zero_X <= not(pd_x(4));
   is_not_zero_Y <= not(pd_y(4));

---------------------------- significand processing ----------------------------
   mantissa_X <= is_not_zero_X & pd_x(0 downto 0);
   mantissa_Y <= is_not_zero_Y & pd_y(0 downto 0);
   significand_product_inst: IntMultiplier_F250_uid53
      port map ( clk  => clk,
                 X => mantissa_X,
                 Y => mantissa_Y,
                 R => significand_product);

------------------------------- scale processing -------------------------------
   scale_X_biased <= pd_x(3 downto 1);
   scale_Y_biased <= pd_y(3 downto 1);
   product_scale_twice_biased <= ("0" & scale_X_biased) + ("0" & scale_Y_biased);

------------------------- significand product shifting -------------------------
   shift_value <= product_scale_twice_biased;
   shift_value_S_bus <= shift_value(3 downto 0);
   significand_product_shifter_inst: LeftShifter4_by_max_8_F250_uid60
      port map ( clk  => clk,
                 S => shift_value_S_bus,
                 X => significand_product,
                 R => shifted_frac);

------------------------ 2's complement of the summand ------------------------
   summand <= shifted_frac(10 downto 2);
   summand1c <= summand when sign_M='0' else not(summand);
   ext_summand1c <= (11 downto 9 => sign_M) & summand1c;

----------------------------- Syncing some signals -----------------------------
   not_ftz <= not FTZ;
   EOB_internal <= EOB;
   not_ftz_sync <= not_ftz;
   carry_0_sync <= sign_M;
   EOB_internal_delayed <= EOB_internal_d1;

---------------------------- Carry Save Accumulator ----------------------------
   -- DQ logic
   acc_0_q <= acc_0_d1;

   -- sequential addition logic
   carry_0 <= carry_0_sync;
   summand_0 <= ext_summand1c(11 downto 0);
   summand_and_carry_0 <= ("0" & summand_0) + carry_0;
   acc_0 <= (("0" & acc_0_q(11 downto 0)) + summand_and_carry_0) when (not_ftz_sync='1') else
            summand_and_carry_0;

-------------------------------- Output Compose --------------------------------
   A <= acc_0_q(11 downto 0);
   EOB_Q <= EOB_internal_delayed;
end architecture;

--------------------------------------------------------------------------------
--                                     PE
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Ledoux Louis - BSC / UPC
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: pd_row_i_A pd_col_j_B C_out SOB EOB
-- Output signals: pd_row_im1_A pd_col_jm1_B SOB_Q EOB_Q C_out_Q

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity PE is
    port (clk, rst : in std_logic;
          pd_row_i_A : in  std_logic_vector(6 downto 0);
          pd_col_j_B : in  std_logic_vector(6 downto 0);
          C_out : in  std_logic_vector(11 downto 0);
          SOB : in  std_logic;
          EOB : in  std_logic;
          pd_row_im1_A : out  std_logic_vector(6 downto 0);
          pd_col_jm1_B : out  std_logic_vector(6 downto 0);
          SOB_Q : out  std_logic;
          EOB_Q : out  std_logic;
          C_out_Q : out  std_logic_vector(11 downto 0)   );
end entity;

architecture arch of PE is
   component pdfdp_4_0_F250_uid51 is
      port ( clk, rst : in std_logic;
             pd_x : in  std_logic_vector(6 downto 0);
             pd_y : in  std_logic_vector(6 downto 0);
             FTZ : in  std_logic;
             EOB : in  std_logic;
             A : out  std_logic_vector(11 downto 0);
             EOB_Q : out  std_logic   );
   end component;

signal pd_row_i_A_q :  std_logic_vector(6 downto 0);
signal pd_col_j_B_q :  std_logic_vector(6 downto 0);
signal sob_delayed :  std_logic;
signal eob_delayed :  std_logic;
signal A_pdfdp :  std_logic_vector(11 downto 0);
signal EOB_sync :  std_logic;
signal mux_C_out, mux_C_out_d1, mux_C_out_d2 :  std_logic_vector(11 downto 0);
signal mux_C_out_HSSD :  std_logic_vector(11 downto 0);
signal pd_row_i_A_d1 :  std_logic_vector(6 downto 0);
signal pd_col_j_B_d1 :  std_logic_vector(6 downto 0);
signal SOB_d1 :  std_logic;
signal EOB_d1 :  std_logic;
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            mux_C_out_d1 <=  mux_C_out;
            mux_C_out_d2 <=  mux_C_out_d1;
            pd_row_i_A_d1 <=  pd_row_i_A;
            pd_col_j_B_d1 <=  pd_col_j_B;
            SOB_d1 <=  SOB;
            EOB_d1 <=  EOB;
         end if;
      end process;
------------------------ Functional delay z-1 of inputs ------------------------
   pd_row_i_A_q <= pd_row_i_A_d1;
   pd_col_j_B_q <= pd_col_j_B_d1;

------------------------- DQ flip flop for SOB and EOB -------------------------
   sob_delayed <= SOB_d1;
   eob_delayed <= EOB_d1;

---------------------------- Instantiates the PDFDP ----------------------------
   pdfdp_inst: pdfdp_4_0_F250_uid51
      port map ( clk  => clk,
                 rst  => rst,
                 EOB => EOB,
                 FTZ => SOB,
                 pd_x => pd_row_i_A,
                 pd_y => pd_col_j_B,
                 A => A_pdfdp,
                 EOB_Q => EOB_sync);

----------------------------- Half Speed Sink Down -----------------------------
   with EOB_sync  select  mux_C_out <= 
        A_pdfdp when '1', 
        C_out when others;
   mux_C_out_HSSD <= mux_C_out_d2;

------------------------- Compose the outputs signals -------------------------
   pd_row_im1_A <= pd_row_i_A_q;
   pd_col_jm1_B <= pd_col_j_B_q;
   SOB_Q <= sob_delayed;
   EOB_Q <= eob_delayed;
   C_out_Q <= mux_C_out_HSSD;
end architecture;

--------------------------------------------------------------------------------
--                            SystolicArrayKernel
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Ledoux Louis - BSC / UPC
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: rowsA colsB SOB EOB
-- Output signals: colsC EOB_Q_o

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity SystolicArrayKernel is
    port (clk, rst : in std_logic;
          rowsA : in  std_logic_vector(13 downto 0);
          colsB : in  std_logic_vector(13 downto 0);
          SOB : in  std_logic;
          EOB : in  std_logic;
          colsC : out  std_logic_vector(23 downto 0);
          EOB_Q_o : out  std_logic   );
end entity;

architecture arch of SystolicArrayKernel is
   component PE is
      port ( clk, rst : in std_logic;
             pd_row_i_A : in  std_logic_vector(6 downto 0);
             pd_col_j_B : in  std_logic_vector(6 downto 0);
             C_out : in  std_logic_vector(11 downto 0);
             SOB : in  std_logic;
             EOB : in  std_logic;
             pd_row_im1_A : out  std_logic_vector(6 downto 0);
             pd_col_jm1_B : out  std_logic_vector(6 downto 0);
             SOB_Q : out  std_logic;
             EOB_Q : out  std_logic;
             C_out_Q : out  std_logic_vector(11 downto 0)   );
   end component;

type T_2D_n_mp1 is array(1 downto 0, 2 downto 0) of std_logic_vector(6 downto 0);
type T_2D_np1_m is array(2 downto 0, 1 downto 0) of std_logic_vector(6 downto 0);
type T_2D_np1_m_logic is array(2 downto 0, 1 downto 0) of std_logic;
type T_2D_quire_np1_m is array(2 downto 0, 1 downto 0) of std_logic_vector(11 downto 0);
signal systolic_wires_rows_2D : T_2D_n_mp1;
signal systolic_wires_cols_2D : T_2D_np1_m;
signal systolic_sob_2D : T_2D_np1_m_logic;
signal systolic_eob_2D : T_2D_np1_m_logic;
signal systolic_C_out_2D : T_2D_quire_np1_m;
begin

----------------- Connect bus of B columns to top edges SA PEs -----------------
   cols_in: for JJ in 0 to 1 generate
      systolic_wires_cols_2D(0,JJ) <= colsB(((JJ+1)*7)-1 downto (JJ*7));
   end generate;

------------------ Connect bus of A rows to left edges SA PEs ------------------
   rows_in: for II in 0 to 1 generate
      systolic_wires_rows_2D(II,0) <= rowsA(((II+1)*7)-1 downto (II*7));
   end generate;

-------------- Connect the Start of Block signals of the TOP PEs --------------
   systolic_sob_2D(0,0) <= SOB;
   sob_1st_row: for JJ in 1 to 1 generate
      systolic_sob_2D(0,JJ) <= systolic_sob_2D(1,JJ-1);
   end generate;

--------------- Connect the End of Block signals of the TOP PEs ---------------
   systolic_eob_2D(0,0) <= EOB;
   eob_1st_row: for JJ in 1 to 1 generate
      systolic_eob_2D(0,JJ) <= systolic_eob_2D(1,JJ-1);
   end generate;

----------- Connect with 0s the input C carry out scheme of TOP PEs -----------
   C_out_input_1st_row: for JJ in 0 to 1 generate
      systolic_C_out_2D(0,JJ) <= "000000000000";
   end generate;

------------------------- Connect PEs locally together -------------------------
   rows: for II in 0 to 1 generate
      cols: for JJ in 0 to 1 generate
         PE_ij: PE
            port map ( clk => clk,
                       rst => rst,
                       pd_row_i_A => systolic_wires_rows_2D(II,JJ),
                       pd_col_j_B => systolic_wires_cols_2D(II,JJ),
                       SOB => systolic_sob_2D(II,JJ),
                       SOB_Q => systolic_sob_2D(II+1,JJ),
                       EOB => systolic_eob_2D(II,JJ),
                       EOB_Q => systolic_eob_2D(II+1,JJ),
                       C_out => systolic_C_out_2D(II,JJ),
                       C_out_Q => systolic_C_out_2D(II+1,JJ),
                       pd_row_im1_A => systolic_wires_rows_2D(II,JJ+1),
                       pd_col_jm1_B => systolic_wires_cols_2D(II+1,JJ));
      end generate;
   end generate;

------------------ Connect last row output C to output C bus ------------------
   cols_C_out: for JJ in 0 to 1 generate
      colsC(((JJ+1)*12)-1 downto (JJ*12)) <= systolic_C_out_2D(2,JJ);
   end generate;

------ Connect PE(N-1,M-1) EOB_Q to out world for valid data computation ------
   EOB_Q_o <= systolic_eob_2D(2,1);

end architecture;

--------------------------------------------------------------------------------
--                               SystolicArray
--                     (SA_orthogonal_2w2h_4_0_F250_uid2)
-- VHDL generated for VirtexUltrascalePlus @ 250MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Ledoux Louis - BSC / UPC
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 4
-- Target frequency (MHz): 250
-- Input signals: rowsA colsB SOB EOB
-- Output signals: colsC EOB_Q_o

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity SystolicArray is
    port (clk, rst : in std_logic;
          rowsA : in  std_logic_vector(7 downto 0);
          colsB : in  std_logic_vector(7 downto 0);
          SOB : in  std_logic;
          EOB : in  std_logic;
          colsC : out  std_logic_vector(7 downto 0);
          EOB_Q_o : out  std_logic   );
end entity;

architecture arch of SystolicArray is
   component Posit2PD is
      port ( clk : in std_logic;
             posit_i : in  std_logic_vector(3 downto 0);
             pd_o : out  std_logic_vector(6 downto 0)   );
   end component;

   component Quire2Posit is
      port ( clk : in std_logic;
             A : in  std_logic_vector(11 downto 0);
             posit_O : out  std_logic_vector(3 downto 0)   );
   end component;

   component SystolicArrayKernel is
      port ( clk, rst : in std_logic;
             rowsA : in  std_logic_vector(13 downto 0);
             colsB : in  std_logic_vector(13 downto 0);
             SOB : in  std_logic;
             EOB : in  std_logic;
             colsC : out  std_logic_vector(23 downto 0);
             EOB_Q_o : out  std_logic   );
   end component;

type array_M_p is array(1 downto 0) of std_logic_vector(3 downto 0);
type array_M_pd is array(1 downto 0) of std_logic_vector(6 downto 0);
type array_N_p is array(1 downto 0) of std_logic_vector(3 downto 0);
type array_N_pd is array(1 downto 0) of std_logic_vector(6 downto 0);
signal posit_row_0 :  std_logic_vector(3 downto 0);
signal posit_row_0_q0 :  std_logic_vector(3 downto 0);
signal posit_row_1, posit_row_1_d1 :  std_logic_vector(3 downto 0);
signal posit_row_1_q1 :  std_logic_vector(3 downto 0);
signal posit_col_0 :  std_logic_vector(3 downto 0);
signal posit_col_0_q0 :  std_logic_vector(3 downto 0);
signal posit_col_1, posit_col_1_d1 :  std_logic_vector(3 downto 0);
signal posit_col_1_q1 :  std_logic_vector(3 downto 0);
signal colsC_Quire :  std_logic_vector(23 downto 0);
signal SOB_select :  std_logic;
signal SOB_q0 :  std_logic;
signal EOB_select :  std_logic;
signal EOB_q0 :  std_logic;
signal q2p :  std_logic_vector(7 downto 0);
signal posit_col_out_0, posit_col_out_0_d1 :  std_logic_vector(3 downto 0);
signal posit_col_out_0_q1 :  std_logic_vector(3 downto 0);
signal posit_col_out_1 :  std_logic_vector(3 downto 0);
signal posit_col_out_1_q0 :  std_logic_vector(3 downto 0);
signal rows_i_p : array_N_p;
signal rows_i_pd :  std_logic_vector(13 downto 0);
signal cols_j_p : array_M_p;
signal cols_j_pd :  std_logic_vector(13 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            posit_row_1_d1 <=  posit_row_1;
            posit_col_1_d1 <=  posit_col_1;
            posit_col_out_0_d1 <=  posit_col_out_0;
         end if;
      end process;
----------------- Delay depending on row index incoming posit -----------------
   posit_row_0 <= rowsA(3 downto 0);
   posit_row_0_q0 <= posit_row_0;
   posit_row_1 <= rowsA(7 downto 4);
   posit_row_1_q1 <= posit_row_1_d1;

----------------- Delay depending on col index incoming posit -----------------
   posit_col_0 <= colsB(3 downto 0);
   posit_col_0_q0 <= posit_col_0;
   posit_col_1 <= colsB(7 downto 4);
   posit_col_1_q1 <= posit_col_1_d1;

---------------- Delay SOB/EOB with Posit2PD delay to feed SAK ----------------
   SOB_select <= SOB;
   SOB_q0 <= SOB_select;
   EOB_select <= EOB;
   EOB_q0 <= EOB_select;

----------------- Delay outgoing posit depending on col index -----------------
   posit_col_out_0 <= q2p(3 downto 0);
   posit_col_out_0_q1 <= posit_col_out_0_d1;
   posit_col_out_1 <= q2p(7 downto 4);
   posit_col_out_1_q0 <= posit_col_out_1;

----------------- Generate Posit2PD for rows and connect them -----------------
   rows_i_p(0) <= posit_row_0_q0;
   rows_i_p(1) <= posit_row_1_q1;
   rows_p2pd: for II in 0 to 1 generate
      p2pd_i: Posit2PD
         port map ( clk => clk,
                    posit_i => rows_i_p(II),
                    pd_o => rows_i_pd(((II+1)*7)-1 downto II*7));
   end generate;

----------------- Generate Posit2PD for cols and connect them -----------------
   cols_j_p(0) <= posit_col_0_q0;
   cols_j_p(1) <= posit_col_1_q1;
   cols_p2pd: for JJ in 0 to 1 generate
      p2pd_j: Posit2PD
         port map ( clk => clk,
                    posit_i => cols_j_p(JJ),
                    pd_o => cols_j_pd(((JJ+1)*7)-1 downto JJ*7));
   end generate;

-------------------- Instantiate the Systolic Array Kernel --------------------
   sak: SystolicArrayKernel
      port map ( clk => clk,
                 rst => rst,
                 rowsA => rows_i_pd,
                 colsB => cols_j_pd,
                 SOB => SOB_q0,
                 EOB => EOB_q0,
                 EOB_Q_o => EOB_Q_o,
                 colsC => colsC_Quire );

----------------------------- Generate Quire2Posit -----------------------------
   cols_quire2posit: for JJ in 0 to 1 generate
      quire2posit_i: Quire2Posit
         port map ( clk => clk,
                    A => colsC_Quire(((JJ+1)*12)-1 downto JJ*12),
                    posit_O => q2p(((JJ+1)*4)-1 downto JJ*4));
   end generate;

------------- Connect outgoing delayed posits to colsC output bus -------------
   colsC(3 downto 0) <= posit_col_out_0_q1;
   colsC(7 downto 4) <= posit_col_out_1_q0;

end architecture;

