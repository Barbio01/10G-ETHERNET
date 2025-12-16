-- EMACS settings: -*- tab-width: 2; indent-tabs-mode: nil -*-
-- vim: tabstop=2:shiftwidth=2:expandtab
-- kate: tab-width 2; replace-tabs on; indent-width 2;
--------------------------------------------------------------------------------
--! @file
--! @brief Generic instantiation of a single or dual clock,
--!   same or mixed-width FIFO
--! @author Steffen Stärz <steffen.staerz@cern.ch>
--------------------------------------------------------------------------------
--! @details Instantiation of a FIFO according to generics.
--!
--! The generics #wr_d_width and #wr_d_depth define the size of the FIFO.
--! The generic #rd_d_width must be a power-of-2 ratio with #wr_d_width.
--! When #rd_d_width \f$ \neq 1\f$ #wr_d_width, #dual_clk must be true.
--! According to those settings, the different FIFO primitives (single clock,
--! dual clock or dual clock mixed width) are instantiated.
--------------------------------------------------------------------------------
--
-- Instantiation template:
--
--  [inst_name]: entity memory.generic_fifo
--  generic map (
--    wr_d_width  => [positive],        --! width of data words
--    wr_d_depth  => [positive],        --! depth of the FIFO
--    rd_d_width  => [natural := 0],    --! width of data words
--    dual_clk    => [boolean := false] --! set to true to enable dual clock FIFO
--    showahead   => [boolean := false] --! enable show-ahead option
--    max_depth   => [integer := 6]     --! maximum block depth (base value)
--    ram_type    => [string  := ""]    --! specify RAM type
--  )
--  port map (
--    rst       => [in    std_logic := '0'],  --! async reset, active high
-- -- write clock domain
--    wr_clk    => [in    std_logic],         --! write clk
--    wr_en     => [in    std_logic],         --! write enable
--    wr_data   => [in    std_logic_vector(WR_D_WIDTH-1 downto 0)],            --! write data
--    wr_usedw  => [out   std_logic_vector(log2ceil(WR_D_DEPTH)-1 downto 0)],  --! used data amount
--    wr_empty  => [out   std_logic],         --! write empty
--    wr_full   => [out   std_logic],         --! write full
-- -- read clock domain
--    rd_clk    => [in    std_logic := '0'],  --! read clk (only used if dual_clk = true)
--    rd_en     => [in    std_logic],         --! read enable
--    rd_data   => [out   std_logic_vector(ite(RD_D_WIDTH = 0, WR_D_WIDTH, RD_D_WIDTH)-1 downto 0)],  --! read data
--    rd_usedw  => [out   std_logic_vector(log2ceil(WR_D_DEPTH * WR_D_WIDTH / ite(RD_D_WIDTH = 0, WR_D_WIDTH, RD_D_WIDTH))-1 downto 0)], --! used data amount
--    rd_empty  => [out   std_logic],         --! read empty
--    rd_full   => [out   std_logic]          --! read full
--  );
--
-------------------------------------------------------------------------------

--! @cond
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
-- library required to include log2ceil for port definitions
library PoC;
use PoC.utils.all;
--! @endcond

entity generic_fifo is
  generic (
    --! Width of (write) data words #wr_data of the FIFO.
    wr_d_width      : positive;
    --! Depth of the FIFO.
    wr_d_depth      : positive;
    --! @brief (optional) width of (read) data words #rd_data of the FIFO.
    --! @details Can be set to instantiate a dual clock FIFO with different
    --! write and read port widths.
    --! Its value must be any power-of-2-ratio with #wr_d_width.
    --! Requires #dual_clk = true.
    rd_d_width      : natural := 0;
    --! Switch to use a dual clock FIFO. If active, #rd_clk must be provided.
    dual_clk        : boolean := false;
    --! Switch to enable the show-ahead mode.
    showahead       : boolean := false;
    --! @brief FIFO optimisation: Maximum block depth.
    --! @details FIFO optimisation: The actual value of the maximum block depth
    --! ranges from 128 to 131072, (2^7 to 2^17).
    --! The default value (6) indicates no selection.
    max_depth       : integer range 6 to 17 := 6;
    --! @brief Number of read synchronization stages.
    rd_sync_stages  : integer := 4;
    --! @brief Number of write synchronization stages.
    wr_sync_stages  : integer := 4;
    --! FIFO optimisation: Indicate which RAM type to use.
    ram_type        : string  := ""
  );
  port (
    --! Reset (asynchronous).
    rst       : in    std_logic  := '0';
    --! Write clock.
    wr_clk    : in    std_logic;
    --! Write enable.
    wr_en     : in    std_logic;
    --! Write data of width #wr_d_width.
    wr_data   : in    std_logic_vector(WR_D_WIDTH-1 downto 0);
    --! Number of used words in the FIFO (synchroneous to #wr_clk).
    wr_usedw  : out   std_logic_vector(log2ceil(WR_D_DEPTH)-1 downto 0);
    --! Indicator that the FIFO is empty (synchroneous to #wr_clk).
    wr_empty  : out   std_logic;
    --! Indicator that the FIFO is full (synchroneous to #wr_clk).
    wr_full   : out   std_logic;
    --! Read clock. Only required if #dual_clk is true.
    rd_clk    : in    std_logic := '0';
    --! Read enable.
    rd_en     : in    std_logic;
    --! Read data of width #wr_d_width (or #rd_d_width if provided).
    rd_data   : out   std_logic_vector(ite(RD_D_WIDTH = 0, WR_D_WIDTH, RD_D_WIDTH)-1 downto 0);
    --! Number of used words in the FIFO (synchroneous to #rd_clk).
    rd_usedw  : out   std_logic_vector(log2ceil(WR_D_DEPTH * WR_D_WIDTH / ite(RD_D_WIDTH = 0, WR_D_WIDTH, RD_D_WIDTH))-1 downto 0);
    --! Indicator that the FIFO is empty (synchroneous to #rd_clk).
    rd_empty  : out   std_logic;
    --! Indicator that the FIFO is full (synchroneous to #rd_clk).
    rd_full   : out   std_logic
  );
end generic_fifo;

-- library required to include VENDOR
--! @cond
library PoC;
use PoC.config.all;
--! @endcond

--! Depending on the vendor detected, either an altera_fifo or a xilinx_fifo is instantiated.
architecture rtl of generic_fifo is

  -- Constants to normalize widths (Single clock implies widths must match)
  constant C_WIDTH : natural := WR_D_WIDTH;
  constant C_DEPTH : natural := WR_D_DEPTH;

  -- Memory Definition
  type mem_t is array (0 to C_DEPTH-1) of std_logic_vector(C_WIDTH-1 downto 0);
  signal mem : mem_t := (others => (others => '0'));

  -- Attribute to guide synthesis tools (e.g., Vivado, Quartus) for RAM type
  attribute ram_style : string;
  attribute ram_style of mem : signal is ram_type;

  -- Pointers and Counters
  -- Using integers for behavioral simplicity and simulation speed
  signal head_ptr   : integer range 0 to C_DEPTH-1 := 0;
  signal tail_ptr   : integer range 0 to C_DEPTH-1 := 0;
  signal count_int  : integer range 0 to C_DEPTH   := 0;

  -- Internal Status Flags
  signal full_i     : std_logic;
  signal empty_i    : std_logic;

begin

  -- ---------------------------------------------------------------------------
  -- Sanity Checks
  -- ---------------------------------------------------------------------------
  -- Ensure this architecture is not used for Dual Clock configurations
  assert not dual_clk
    report "Generic FIFO: This architecture (rtl) is for Single Clock only. Set dual_clk => false."
    severity failure;

  -- Ensure widths match (SC FIFO usually implies same width R/W)
  assert (rd_d_width = 0) or (rd_d_width = wr_d_width)
    report "Generic FIFO: Single Clock mode requires rd_d_width to be 0 or equal to wr_d_width."
    severity failure;

  -- ---------------------------------------------------------------------------
  -- Status Logic
  -- ---------------------------------------------------------------------------
  full_i  <= '1' when count_int = C_DEPTH else '0';
  empty_i <= '1' when count_int = 0 else '0';

  -- Map internal flags to output ports
  wr_full  <= full_i;
  wr_empty <= empty_i;
  wr_usedw <= std_logic_vector(to_unsigned(count_int, wr_usedw'length));

  -- In Single Clock mode, read status ports mirror write status ports
  rd_full  <= full_i;
  rd_empty <= empty_i;
  rd_usedw <= std_logic_vector(to_unsigned(count_int, rd_usedw'length));

  -- ---------------------------------------------------------------------------
  -- FIFO Control Process
  -- ---------------------------------------------------------------------------
  process(wr_clk, rst)
  begin
    if rst = '1' then
      head_ptr  <= 0;
      tail_ptr  <= 0;
      count_int <= 0;

      -- Reset Data Output if not showahead (Standard Mode)
      if not showahead then
        rd_data <= (others => '0');
      end if;

    elsif rising_edge(wr_clk) then

      -- 1. WRITE OPERATION
      -- Write if enabled and not full
      if wr_en = '1' and full_i = '0' then
        mem(head_ptr) <= wr_data;

        if head_ptr = C_DEPTH - 1 then
          head_ptr <= 0;
        else
          head_ptr <= head_ptr + 1;
        end if;
      end if;

      -- 2. READ OPERATION POINTER UPDATE
      -- Update tail pointer if read enabled and not empty
      if rd_en = '1' and empty_i = '0' then
        if tail_ptr = C_DEPTH - 1 then
          tail_ptr <= 0;
        else
          tail_ptr <= tail_ptr + 1;
        end if;
      end if;

      -- 3. COUNT UPDATE
      -- If Write happens but no Read (or Read invalid due to empty)
      if (wr_en = '1' and full_i = '0') and (rd_en = '0' or empty_i = '1') then
        count_int <= count_int + 1;

      -- If Read happens but no Write (or Write invalid due to full)
      elsif (rd_en = '1' and empty_i = '0') and (wr_en = '0' or full_i = '1') then
        count_int <= count_int - 1;
      end if;
      -- If both happen simultaneously, count_int remains unchanged.

      -- 4. STANDARD READ DATA (Registered Output)
      -- If showahead is FALSE, data appears 1 cycle after rd_en
      if not showahead then
        if rd_en = '1' and empty_i = '0' then
          mem(tail_ptr) <= wr_data; -- Pass-through check not needed due to synchronous RAM nature usually
          rd_data <= mem(tail_ptr);
        end if;
      end if;

    end if;
  end process;

  -- ---------------------------------------------------------------------------
  -- Show-Ahead Logic (FWFT)
  -- ---------------------------------------------------------------------------
  -- If showahead is TRUE, data must be valid immediately (Combinatorial Read).
  -- Note: This infers Distributed RAM (LUTRAM) on most FPGAs because BlockRAM
  -- does not support asynchronous reads.
  gen_fwft : if showahead generate
    rd_data <= mem(tail_ptr);
  end generate;

end rtl;