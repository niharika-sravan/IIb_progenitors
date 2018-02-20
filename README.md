# IIb_progenitors

co-author: [Pablo Marchant](https://github.com/orlox) 

`svn export https://github.com/niharika87/IIb_1 <local path>` to download everything

Note: 
1. These files were written for MESA release 9575
2. Assuming you have MESA installed and the shell knows where to find MESA (MESA_DIR definition exported) and, if applicable, MESASDK is sourced

To run:
1. Copy template (binary or single; solar or SMC metallicity) to a new location (`<dest path>`)
2. Update inlist_extra with run parameters using the file in the template as an example
3. `cd <dest path>`
4. `./clean && ./mk && ./rn`
