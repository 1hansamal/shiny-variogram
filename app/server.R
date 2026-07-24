function(input, output, session) {
  uploaded_data <- FileUploadServer("upload_files")

  VariogramServer("variogram", reactive(uploaded_data()))
}