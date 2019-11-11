# generate temp files from list
generate_plot_zip <- function(zip_file, plot_list){
  shiny::withProgress(
    message = paste0("Zipping the plots"),
    value = 0,
    {
      tmp <- tempfile()
      on.exit(unlink(tmp))
      dir.create(tmp)
      file_name_array = c()
      for(i in 1:length(plot_list)){
        shiny::incProgress( 1/length(plot_list)  )
        
        plot_file_name = file.path(tmp, names(plot_list)[i]  )
        on.exit(unlink(plot_file_name))
        ggsave(plot_list[[i]], file = plot_file_name)
        print(plot_file_name)
        file_name_array = c(file_name_array, plot_file_name)
      }
      zipr(zip_file, file_name_array)
    }
  )
}

# generate temp files from list
generate_plot_zip_simple <- function(zip_file, plot_list){
      tmp <- tempfile()
      on.exit(unlink(tmp))
      dir.create(tmp)
      file_name_array = c()
      for(i in 1:length(plot_list)){
        
        plot_file_name = file.path(tmp, names(plot_list)[i]  )
        on.exit(unlink(plot_file_name))
        ggsave(plot_list[[i]], file = plot_file_name)
        print(plot_file_name)
        file_name_array = c(file_name_array, plot_file_name)
      }
      zipr(zip_file, file_name_array)

}