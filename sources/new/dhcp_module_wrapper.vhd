library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;  -- Defines std_logic and std_logic_vector
    use IEEE.NUMERIC_STD.ALL;

library PoC;
    use PoC.utils.all;

library fpga;
--context fpga.constants;
    use fpga.fpga_if.all;

entity dhcp_module_wrapper is
  generic (
    --! UDP CRC calculation enable
    UDP_CRC_EN : boolean := true
  );
  port (
    --! Clock and Reset
    clk             : in  std_logic;
    rst             : in  std_logic;
    boot_i          : in  std_logic := '0';

    --! @name RX Interface (Flattened t_avst_packet)
    --! @{
    dhcp_rx_ready_o : out std_logic;
    dhcp_rx_valid   : in  std_logic;
    dhcp_rx_sop     : in  std_logic;
    dhcp_rx_eop     : in  std_logic;
    dhcp_rx_data    : in  std_logic_vector(63 downto 0);
    dhcp_rx_empty   : in  std_logic_vector(2 downto 0);
    dhcp_rx_error   : in  std_logic_vector(0 downto 0);
    --! @}

    --! @name TX Interface (Flattened t_avst_packet)
    --! @{
    dhcp_tx_ready_i : in  std_logic;
    dhcp_tx_valid   : out std_logic;
    dhcp_tx_sop     : out std_logic;
    dhcp_tx_eop     : out std_logic;
    dhcp_tx_data    : out std_logic_vector(63 downto 0);
    dhcp_tx_empty   : out std_logic_vector(2 downto 0);
    dhcp_tx_error   : out std_logic_vector(0 downto 0);
    dhcp_server_ip_o: out std_logic_vector(31 downto 0);
    --! @}

    --! @name MAC Recovery Interface
    --! @{
    reco_en_o       : out std_logic;
    reco_ip_o       : out std_logic_vector(31 downto 0);
    reco_done_i     : in  std_logic;
    reco_fail_i     : in  std_logic;
    --! @}

    --! @name Configuration & Status
    --! @{
    my_mac_i        : in  std_logic_vector(47 downto 0);
    try_ip_i        : in  std_logic_vector(31 downto 0) := (others => '0');
    my_ip_o         : out std_logic_vector(31 downto 0);
    ip_netmask_o    : out std_logic_vector(31 downto 0);
    one_ms_tick_i   : in  std_logic;
    status_vector_o : out std_logic_vector(6 downto 0)
    --! @}
  );
end entity dhcp_module_wrapper;

architecture rtl of dhcp_module_wrapper is

  -- Internal signals to bridge the standard ports to the VHDL-2008 record types
  signal rx_packet_s : t_avst_packet(data(63 downto 0), empty(2 downto 0), error(0 downto 0));
  signal tx_packet_s : t_avst_packet(data(63 downto 0), empty(2 downto 0), error(0 downto 0));

begin

  -- Map Flattened Inputs to Record Signal
  rx_packet_s.valid <= dhcp_rx_valid;
  rx_packet_s.sop   <= dhcp_rx_sop;
  rx_packet_s.eop   <= dhcp_rx_eop;
  rx_packet_s.data  <= dhcp_rx_data;
  rx_packet_s.empty <= dhcp_rx_empty;
  rx_packet_s.error <= dhcp_rx_error;

  -- Map Record Signal to Flattened Outputs
  dhcp_tx_valid <= tx_packet_s.valid;
  dhcp_tx_sop   <= tx_packet_s.sop;
  dhcp_tx_eop   <= tx_packet_s.eop;
  dhcp_tx_data  <= tx_packet_s.data;
  dhcp_tx_empty <= tx_packet_s.empty;
  dhcp_tx_error <= tx_packet_s.error;

  -- Instantiate the VHDL-2008 Entity
  -- We use Direct Entity Instantiation to avoid component declaration mismatches
  inst_dhcp_module : entity work.dhcp_module
    generic map (
      UDP_CRC_EN => UDP_CRC_EN
    )
    port map (
      clk               => clk,
      rst               => rst,
      boot_i            => boot_i,
      
      -- RX
      dhcp_rx_ready_o   => dhcp_rx_ready_o,
      dhcp_rx_packet_i  => rx_packet_s,
      
      -- TX
      dhcp_tx_ready_i   => dhcp_tx_ready_i,
      dhcp_tx_packet_o  => tx_packet_s,
      dhcp_server_ip_o  => dhcp_server_ip_o,
      
      -- Recovery
      reco_en_o         => reco_en_o,
      reco_ip_o         => reco_ip_o,
      reco_done_i       => reco_done_i,
      reco_fail_i       => reco_fail_i,
      
      -- Config
      my_mac_i          => my_mac_i,
      try_ip_i          => try_ip_i,
      my_ip_o           => my_ip_o,
      ip_netmask_o      => ip_netmask_o,
      one_ms_tick_i     => one_ms_tick_i,
      status_vector_o   => status_vector_o
    );

end architecture rtl;