# KDA Miner (WIP)

A Standalone kadena GPU miner.

Heavily based on https://github.com/kadena-io/chainweb-miner and https://github.com/kadena-community/bigolchungus

# Installation

Currently only Ubuntu 16.04 and 18.04 have releases. 
Building from source should be trivial via `stack install`

You can find releases here: https://github.com/Jacoby6000/kda-miner/releases

# Example usage:

```bash
kda-miner mine \
  --node kawaii.waifuwars.org:35000 \
  --log-level debug \
  --miner-account JayKobe6k \
  --miner-key 84811e7773ec9f6546d8baaf48c79119414b4bed3bfe752c82af6326e5d6b7ff
```

You may specify more than one node.

If you specify no devices, all of your available GPUs will be used.

If you wish to specify devices, you can do so via `--device 0,0 --device 0,1` and so on.  
The first number represents the platform, and the second number represents the devices id.  
You can run `kda-miner show-devices` to see your available GPUs


There is currently a 10% dev fee in the binaries provided in release.  
This will donate 10 minutes of your GPU hashing for every 100 minutes it runs.

The dev account being mined for is `88a56a0b99d6cd89a041bae00b58a10832453143c924cd00d3a83e1dc076ee0c`
