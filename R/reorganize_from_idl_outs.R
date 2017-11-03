in_folder  <- "~/nr_working/shared/PhenoRice/Processing/Senegal/Final/Outputs_MaskIrrig/"
out_folder <- "~/nr_working/shared/PhenoRice/Processing/Senegal/Final/Outputs_MaskIrrig/tiffs"
pr_extract_idl_outs(in_folder, out_folder, 2003,2016)

pr_create_ordered_tiffs(out_folder,
                        out_folder = "~/nr_working/shared/PhenoRice/Processing/Senegal/Final/Outputs_MaskIrrig/tiffs/multi",
                        patterns = c("nseas", "sos", "pos", "cumevi", "veglgt", "tot_lgt"))
list.files(in_folder)

