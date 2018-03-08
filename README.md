# IIb_progenitors

co-author: [Pablo Marchant](https://github.com/orlox) 

`svn export https://github.com/niharika-sravan/IIb_progenitors <local path>` to download everything

Note: 
1. These files were written for MESA release 9575
2. I assume you have MESA installed, the shell knows where to find MESA (i.e. MESA_DIR definition is exported) and, if applicable, MESASDK is sourced

To run:
1. Copy the template of the type of model you want to run (binary or single; solar or quarter-solar (SMC) metallicity) to a new location (`<dest path>`)
2. Update inlist_extra with parameters for the model using the `inlist_extra` in the template folder as an example
3. `cd <dest path>`
4. `./clean && ./mk && ./rn`
