

MSSG <- function(language){
  lapply(list(
    created_repository =
      data.frame(
        en = "Created repository",
        pt = "Criado repositorio",
        es = "Creado repositorio"),
    created_new_repository_version =
      data.frame(
        en = "Created new version of repository",
        pt = "Criada nova versao do repositorio",
        es = "Creada nueva version del repositorio"),
    more_than_one_deposition =
      data.frame(
        en = "More than one deposition named",
        pt = "Existe mais do que uma deposicao chamada",
        es = "Existe mas de una deposicion llamada"
      ),
    exists_en =
      data.frame(
        en = " exists",
        pt = "",
        es = ""
      ),
    check_server_for_duplicates =
      data.frame(
        en = "Check the server for duplicates",
        pt = "Verifique o servidor em busca de duplicados",
        es = "Verifique el servidor en busca de duplicados"
      ),
    downloading_files_from =
      data.frame(
        en = "Downloading all files from",
        pt = "Descarregando todos os ficheiros de",
        es = "Descargando todos los archivos de"
      ),

    downloading =
      data.frame(
        en = "Downloading",
        pt = "Descarregando",
        es = "Descargando"
      ),
    uploading_file =
      data.frame(
        en = "Uploading file",
        pt = "Enviando arquivo",
        es = "Subiendo archivo"
      ),
    to =
      data.frame(
        en = "to",
        pt = "para",
        es = "a"
      ),

    deleted_file =
      data.frame(
        en = "Deleted file",
        pt = "Apagado arquivo",
        es = "Borrado archivo"
      ),
    from =
      data.frame(
        en = "from",
        pt = "de",
        es = "de"
      )


     ), function(x) x[1,which(colnames(x) == language)])
}

