
-------------------------------------------------------------------------------
-- (c) Copyright - John Tuthill
-- 2016
--
-- All Rights Reserved.
--
-- Restricted Use.
--
-- Copyright protects this code. Except as permitted by the Copyright Act, you
-- may only use the code as expressly permitted under the terms on which the
-- code was licensed to you.
--
-------------------------------------------------------------------------------
--   File Name:             coincidence_det.vhd
--   Type:                  RTL
--   Contributing authors:  J. Tuthill
--   Created:               Mon Jan 25 17:24:59 2016
--   Template Rev:          1.0
--
--   Title:                 Coincidence detector top-level.
--   Description: 
--   
--   
--
--   
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;



-------------------------------------------------------------------------------
entity coincidence_det is
   port (

      clk : in std_logic;

      -- GM tube inputs
      tube_1 : in std_logic;
      tube_2 : in std_logic;
      tube_3 : in std_logic;

      -- Individual LED outputs
      led_out : out std_logic_vector(7 downto 0);

      -- 7-segment displays
      an     : out std_logic_vector(3 downto 0);
      seg_a  : out std_logic;
      seg_b  : out std_logic;
      seg_c  : out std_logic;
      seg_d  : out std_logic;
      seg_e  : out std_logic;
      seg_f  : out std_logic;
      seg_g  : out std_logic;
      seg_dp : out std_logic;

      -- buzzer
      piezo : out std_logic;

      -- Push-button
      pb0 : in std_logic;
      pb1 : in std_logic


      );
end coincidence_det;

-------------------------------------------------------------------------------
architecture rtl of coincidence_det is


   ---------------------------------------------------------------------------
   --                 CONSTANT, TYPE AND GENERIC DEFINITIONS                --
   ---------------------------------------------------------------------------
   type array_16_by_7_t is array(0 to 15) of std_logic_vector(6 downto 0);

   constant disp_cnt_c               : integer := 5000000;
   constant coincidence_wndw_width_c : integer := 75;  -- 1.5us coincidence window
   constant bcd_seven_seg_decode_c   : array_16_by_7_t :=
      ("0000001",
       "1001111",
       "0010010",
       "0000110",
       "1001100",
       "0100100",
       "0100000",
       "0001111",
       "0000000",
       "0000100",
       "0001000",
       "1100000",
       "0110001",
       "1000010",
       "0110000",
       "0111000");
   constant debounce_count_c : std_logic_vector(24 downto 0) := "1000000000000000000000000";
   ---------------------------------------------------------------------------
   --                          SIGNAL DECLARATIONS                          --
   ---------------------------------------------------------------------------

   signal counter             : std_logic_vector(31 downto 0);
   signal tube_1_reg          : std_logic := '0';
   signal tube_2_reg          : std_logic := '0';
   signal tube_3_reg          : std_logic := '0';
   signal tube_1_reg_d1       : std_logic := '0';
   signal tube_2_reg_d1       : std_logic := '0';
   signal tube_3_reg_d1       : std_logic := '0';
   signal tube_1_rising       : std_logic;
   signal tube_2_rising       : std_logic;
   signal tube_3_rising       : std_logic;
   signal tube_1_rising_latch : std_logic := '0';
   signal tube_2_rising_latch : std_logic := '0';
   signal tube_3_rising_latch : std_logic := '0';
   signal any_tube_rising     : std_logic;

   signal tube_1_cnt         : std_logic_vector(23 downto 0) := (others => '0');
   signal tube_1_cnt_en      : std_logic                     := '0';
   signal tube_1_cnt_reached : std_logic;
   signal tube_2_cnt         : std_logic_vector(23 downto 0) := (others => '0');
   signal tube_2_cnt_en      : std_logic                     := '0';
   signal tube_2_cnt_reached : std_logic;
   signal tube_3_cnt         : std_logic_vector(23 downto 0) := (others => '0');
   signal tube_3_cnt_en      : std_logic                     := '0';
   signal tube_3_cnt_reached : std_logic;

   signal coincidence_cnt            : std_logic_vector(7 downto 0) := "00000000";
   signal coincidence_cnt_en         : std_logic                    := '0';
   signal coincidence_cnt_reached    : std_logic;
   signal coincidence_cnt_reached_d1 : std_logic                    := '0';

   signal coincidence_true        : std_logic;
   signal coincidence_true_d1     : std_logic                    := '0';
   signal coincidence_rising      : std_logic;
   signal coincidence_rising_pipe : std_logic_vector(7 downto 0) := "00000000";

   signal coincidence_cnt_7_seg : std_logic_vector(6 downto 0) := (others => '1');

   signal strobe_interval_cnt : std_logic_vector(17 downto 0) := (others => '0');
   signal strobe_cnt          : std_logic_vector(1 downto 0)  := "00";

   signal units_cnt      : std_logic_vector(3 downto 0) := "0000";
   signal tens_cnt       : std_logic_vector(3 downto 0) := "0000";
   signal hund_cnt       : std_logic_vector(3 downto 0) := "0000";
   signal thous_cnt      : std_logic_vector(3 downto 0) := "0000";
   signal disp_units_cnt : std_logic_vector(3 downto 0) := "0000";
   signal disp_tens_cnt  : std_logic_vector(3 downto 0) := "0000";
   signal disp_hund_cnt  : std_logic_vector(3 downto 0) := "0000";
   signal disp_thous_cnt : std_logic_vector(3 downto 0) := "0000";

   signal pb0_reg             : std_logic                     := '0';
   signal pb0_reg_d1          : std_logic                     := '0';
   signal pb0_rising          : std_logic;
   signal pb1_reg             : std_logic                     := '0';
   signal pb1_reg_d1          : std_logic                     := '0';
   signal pb1_rising          : std_logic;
   signal debounce_cnt_en     : std_logic                     := '0';
   signal debounce_cnt_rst    : std_logic;
   signal debounce_cnt_rst_d1 : std_logic;
   signal debounce_cnt        : std_logic_vector(24 downto 0) := (others => '0');

   signal disp_function_rand : std_logic                    := '0';
   signal clk_div_4_shift    : std_logic_vector(3 downto 0) := "0001";

   signal enable_beep            : std_logic                     := '0';
   signal beep_freq_cnt          : std_logic_vector(15 downto 0) := (others => '0');
   signal beep_timer_cnt_en      : std_logic                     := '0';
   signal beep_timer_cnt_reached : std_logic;
   signal beep_timer_cnt         : std_logic_vector(23 downto 0) := (others => '0');

   signal seven_seg_an : std_logic_vector(3 downto 0) := "1111";
   signal seven_seg_a  : std_logic                    := '1';
   signal seven_seg_b  : std_logic                    := '1';
   signal seven_seg_c  : std_logic                    := '1';
   signal seven_seg_d  : std_logic                    := '1';
   signal seven_seg_e  : std_logic                    := '1';
   signal seven_seg_f  : std_logic                    := '1';
   signal seven_seg_g  : std_logic                    := '1';
   signal seven_seg_dp : std_logic                    := '1';


   ---------------------------------------------------------------------------
   --                        COMPONENT DECLARATIONS                         --
   ---------------------------------------------------------------------------

   

begin


   ---------------------------------------------------------------------------
   --                    INSTANTIATE COMPONENTS                             --
   ---------------------------------------------------------------------------


   ---------------------------------------------------------------------------
   --                      CONCURRENT SIGNAL ASSIGNMENTS                    --
   ---------------------------------------------------------------------------
   tube_1_rising   <= tube_1_reg and not(tube_1_reg_d1);
   tube_2_rising   <= tube_2_reg and not(tube_2_reg_d1);
   tube_3_rising   <= tube_3_reg and not(tube_3_reg_d1);
   any_tube_rising <= tube_1_rising or tube_2_rising or tube_3_rising;

   tube_1_cnt_reached <= '1' when (tube_1_cnt = std_logic_vector(to_unsigned(disp_cnt_c, 24))) else
                         '0';
   tube_2_cnt_reached <= '1' when (tube_2_cnt = std_logic_vector(to_unsigned(disp_cnt_c, 24))) else
                         '0';
   tube_3_cnt_reached <= '1' when (tube_3_cnt = std_logic_vector(to_unsigned(disp_cnt_c, 24))) else
                         '0';

   -- coincidence detect logic
   coincidence_cnt_reached <= '1' when (coincidence_cnt = std_logic_vector(to_unsigned(coincidence_wndw_width_c, 8))) else
                              '0';

   coincidence_true   <= tube_1_rising_latch and tube_2_rising_latch and tube_3_rising_latch;
   coincidence_rising <= coincidence_true and not(coincidence_true_d1);

   -- LED display signals
   led_out(0)          <= tube_1_cnt_en;
   led_out(1)          <= tube_2_cnt_en;
   led_out(2)          <= tube_3_cnt_en;
   led_out(6)          <= enable_beep;
   led_out(7)          <= disp_function_rand;
   led_out(5 downto 3) <= (others => '0');

   -- push-button logic
   pb0_rising       <= (pb0_reg and not(pb0_reg_d1)) and not(debounce_cnt_en);
   pb1_rising       <= (pb1_reg and not(pb1_reg_d1)) and not(debounce_cnt_en);
   debounce_cnt_rst <= '1' when (debounce_cnt = debounce_count_c) else
                       '0';

   -- assign 7-segment display signals
   seven_seg_a <= coincidence_cnt_7_seg(6);
   seven_seg_b <= coincidence_cnt_7_seg(5);
   seven_seg_c <= coincidence_cnt_7_seg(4);
   seven_seg_d <= coincidence_cnt_7_seg(3);
   seven_seg_e <= coincidence_cnt_7_seg(2);
   seven_seg_f <= coincidence_cnt_7_seg(1);
   seven_seg_g <= coincidence_cnt_7_seg(0);
   an          <= seven_seg_an;
   seg_a       <= seven_seg_a;
   seg_b       <= seven_seg_b;
   seg_c       <= seven_seg_c;
   seg_d       <= seven_seg_d;
   seg_e       <= seven_seg_e;
   seg_f       <= seven_seg_f;
   seg_g       <= seven_seg_g;
   seg_dp      <= seven_seg_dp;

   -- beep output logic
   beep_timer_cnt_reached <= '1' when (beep_timer_cnt = std_logic_vector(to_unsigned(disp_cnt_c, 24))) else
                             '0';
   piezo <= beep_timer_cnt_en and beep_freq_cnt(15);


   ---------------------------------------------------------------------------
   --                         CONCURRENT PROCESSES                          --
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Process:  S3E_SP_PROCESS  
   --  Purpose:  
   --  Inputs:   
   --  Outputs:  
   ---------------------------------------------------------------------------
   coincidence_det_proc : process(clk)

   begin
      
      if rising_edge(clk) then

         -- register input GM tube signals
         tube_1_reg    <= tube_1;
         tube_2_reg    <= tube_2;
         tube_3_reg    <= tube_3;
         tube_1_reg_d1 <= tube_1_reg;
         tube_2_reg_d1 <= tube_2_reg;
         tube_3_reg_d1 <= tube_3_reg;

         --individual CM tube display counters
         if (tube_1_cnt_reached = '1') then
            tube_1_cnt_en <= '0';
         elsif (tube_1_rising = '1') then
            tube_1_cnt_en <= '1';
         else
            tube_1_cnt_en <= tube_1_cnt_en;
         end if;

         if (tube_1_cnt_en = '0') then
            tube_1_cnt <= (others => '0');
         elsif (tube_1_cnt_en = '1') then
            tube_1_cnt <= tube_1_cnt + 1;
         else
            tube_1_cnt <= tube_1_cnt;
         end if;

         if (tube_2_cnt_reached = '1') then
            tube_2_cnt_en <= '0';
         elsif (tube_2_rising = '1') then
            tube_2_cnt_en <= '1';
         else
            tube_2_cnt_en <= tube_2_cnt_en;
         end if;

         if (tube_2_cnt_en = '0') then
            tube_2_cnt <= (others => '0');
         elsif (tube_2_cnt_en = '1') then
            tube_2_cnt <= tube_2_cnt + 1;
         else
            tube_2_cnt <= tube_2_cnt;
         end if;

         if (tube_3_cnt_reached = '1') then
            tube_3_cnt_en <= '0';
         elsif (tube_3_rising = '1') then
            tube_3_cnt_en <= '1';
         else
            tube_3_cnt_en <= tube_3_cnt_en;
         end if;

         if (tube_3_cnt_en = '0') then
            tube_3_cnt <= (others => '0');
         elsif (tube_3_cnt_en = '1') then
            tube_3_cnt <= tube_3_cnt + 1;
         else
            tube_3_cnt <= tube_3_cnt;
         end if;

         -- coincidence detect logic
         coincidence_cnt_reached_d1 <= coincidence_cnt_reached;

         if (coincidence_cnt_reached = '1') then
            tube_1_rising_latch <= '0';
         elsif (tube_1_rising = '1') then
            tube_1_rising_latch <= '1';
         else
            tube_1_rising_latch <= tube_1_rising_latch;
         end if;

         if (coincidence_cnt_reached = '1') then
            tube_2_rising_latch <= '0';
         elsif (tube_2_rising = '1') then
            tube_2_rising_latch <= '1';
         else
            tube_2_rising_latch <= tube_2_rising_latch;
         end if;

         if (coincidence_cnt_reached = '1') then
            tube_3_rising_latch <= '0';
         elsif (tube_3_rising = '1') then
            tube_3_rising_latch <= '1';
         else
            tube_3_rising_latch <= tube_3_rising_latch;
         end if;

         if (coincidence_cnt_reached = '1') then
            coincidence_cnt_en <= '0';
         elsif ((tube_1_rising = '1') or (tube_2_rising = '1') or (tube_2_rising = '1')) then
            coincidence_cnt_en <= '1';
         else
            coincidence_cnt_en <= coincidence_cnt_en;
         end if;

         if (coincidence_cnt_reached_d1 = '1') then
            coincidence_cnt <= (others => '0');
         elsif (coincidence_cnt_en = '1') then
            coincidence_cnt <= coincidence_cnt + 1;
         else
            coincidence_cnt <= coincidence_cnt;
         end if;

         -- clock divider for random number display
         -- this provides a brief 4-clock delay for the units, tens, hundreds
         -- and thousands to settle before displaying them
         clk_div_4_shift(0) <= clk_div_4_shift(3);
         for i in 1 to 3 loop
            clk_div_4_shift(i) <= clk_div_4_shift(i-1);
         end loop;

         -- 7-segment LED display coincidence event counter logic
         coincidence_true_d1        <= coincidence_true;
         coincidence_rising_pipe(0) <= coincidence_rising;
         for i in 1 to 7 loop
            coincidence_rising_pipe(i) <= coincidence_rising_pipe(i-1);
         end loop;
         if (units_cnt >= "1010") then  -- reset units when = 10 and increment tens
            units_cnt <= "0000";
            tens_cnt  <= tens_cnt + 1;
         elsif ((coincidence_rising = '1') or ((disp_function_rand = '1') and (clk_div_4_shift(0) = '1'))) then
            units_cnt <= units_cnt + 1;
         else
            units_cnt <= units_cnt;
         end if;

         if (tens_cnt >= "1010") then
            tens_cnt <= "0000";
            hund_cnt <= hund_cnt + 1;
         end if;

         if (hund_cnt >= "1010") then
            hund_cnt  <= "0000";
            thous_cnt <= thous_cnt + 1;
         end if;

         if (thous_cnt >= "1010") then
            thous_cnt <= "0000";
         end if;

         if (((disp_function_rand = '0') and (coincidence_rising_pipe(7) = '1')) or
             ((disp_function_rand = '1') and (clk_div_4_shift(3) = '1') and (any_tube_rising = '1'))) then
            disp_units_cnt <= units_cnt;
            disp_tens_cnt  <= tens_cnt;
            disp_hund_cnt  <= hund_cnt;
            disp_thous_cnt <= thous_cnt;
         end if;


         strobe_interval_cnt <= strobe_interval_cnt + 1;

         -- strobe the 1 anodes of the 7-segmant displays
         if (strobe_interval_cnt = "000000000000000000") then
            strobe_cnt <= strobe_cnt + 1;
         else
            strobe_cnt <= strobe_cnt;
         end if;

         case strobe_cnt is
            when "00" =>
               seven_seg_an          <= "1110";
               coincidence_cnt_7_seg <= bcd_seven_seg_decode_c(to_integer(unsigned(disp_units_cnt)));

            when "01" =>
               seven_seg_an          <= "1101";
               coincidence_cnt_7_seg <= bcd_seven_seg_decode_c(to_integer(unsigned(disp_tens_cnt)));

            when "10" =>
               seven_seg_an          <= "1011";
               coincidence_cnt_7_seg <= bcd_seven_seg_decode_c(to_integer(unsigned(disp_hund_cnt)));
               
            when "11" =>
               seven_seg_an          <= "0111";
               coincidence_cnt_7_seg <= bcd_seven_seg_decode_c(to_integer(unsigned(disp_thous_cnt)));

            when others =>
               seven_seg_an          <= "1111";
               coincidence_cnt_7_seg <= "1111111";

         end case;

         -- register and debounce the push-buttons
         pb0_reg             <= pb0;
         pb0_reg_d1          <= pb0_reg;
         pb1_reg             <= pb1;
         pb1_reg_d1          <= pb1_reg;
         debounce_cnt_rst_d1 <= debounce_cnt_rst;
         if (debounce_cnt_rst = '1') then
            debounce_cnt_en <= '0';
         elsif ((pb0_rising = '1') or (pb1_rising = '1')) then
            debounce_cnt_en <= '1';
         else
            debounce_cnt_en <= debounce_cnt_en;
         end if;

         if (debounce_cnt_rst_d1 = '1') then
            debounce_cnt <= (others => '0');
         elsif (debounce_cnt_en = '1') then
            debounce_cnt <= debounce_cnt + 1;
         else
            debounce_cnt <= debounce_cnt;
         end if;

         -- toggle the display function on a button-press of pb0
         if (pb0_rising = '1') then
            disp_function_rand <= not(disp_function_rand);
            units_cnt          <= "0000";
            tens_cnt           <= "0000";
            hund_cnt           <= "0000";
            thous_cnt          <= "0000";
         end if;

         -- toggle the beep enable on a button-press of pb1
         if (pb1_rising = '1') then
            enable_beep       <= not(enable_beep);
            beep_timer_cnt_en <= '0';
         end if;

         if (beep_timer_cnt_reached = '1') then
            beep_timer_cnt_en <= '0';
         elsif ((coincidence_true = '1') and (enable_beep = '1')) then
            beep_timer_cnt_en <= '1';
         else
            beep_timer_cnt_en <= beep_timer_cnt_en;
         end if;

         if (beep_timer_cnt_en = '0') then
            beep_timer_cnt <= (others => '0');
         elsif (beep_timer_cnt_en = '1') then
            beep_timer_cnt <= beep_timer_cnt + 1;
         else
            beep_timer_cnt <= beep_timer_cnt;
         end if;
         beep_freq_cnt <= beep_freq_cnt + 1;

         counter <= counter + 1;
         
      end if;
      
   end process coincidence_det_proc;


end rtl;
-------------------------------------------------------------------------------



