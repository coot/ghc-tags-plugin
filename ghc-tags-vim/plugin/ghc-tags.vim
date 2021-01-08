" Update `ghc-tags-plugin` version in a `cabal.project` file
"
fun! UpdateGhcTagsPlugin(ghcVersion) abort
  let view = winsaveview()
  if empty(a:ghcVersion)
    let ghcVersion = matchstr(system("ghc --numeric-version"), '[0-9.]*')
  else
    let ghcVersion = a:ghcVersion
  endif

  let output = system("ghc-pkg-".ghcVersion." --package-db=".$HOME."/.cabal/store/ghc-".ghcVersion."/package.db describe ghc-tags-plugin")
  let ghcTagsPluginIds = filter(matchlist(output, 'id:\_s*\zs\(.\{-}\)\n')[:1], {id, v -> !empty(v)})
  if !empty(ghcTagsPluginIds)
    let v = sort(ghcTagsPluginIds, "GTPCompareIds")[0]
    try
    exe ":%s/\\(-plugin-package-id\\s*=\\s*\\)\\@<=ghc-tags-plugin.*/".escape(v, "./")
    catch /E486:/
    endtry
  endif
  let latest = substitute(system("ghc-pkg --package-db=".$HOME."/.cabal/store/ghc-".ghcVersion."/package.db latest ghc-tags-plugin"), '\_s*$', "", "")
  if !empty(latest)
    try
    exe ':%s/\(-plugin-package\s*=\s*\)\@<=ghc-tags-plugin.*/'.escape(latest, "./")
    catch /E486:/
    endtry
  endif
  call winrestview(view)
endfun

fun! GTPExtractVersion(id)
  return map(split(split(substitute(a:id, '^ghc-tags-plugin-', '', ''), "-")[0], '\.'), {_, val -> str2nr(val) })
endfun

fun! GTPCompareIds(id_0, id_1)
  let ver_0 = GTPExtractVersion(a:id_0)
  let ver_1 = GTPExtractVersion(a:id_1)
  if [ver_0, ver_1] == sort([ver_0, ver_1])
    return 1
  else
    return -1
  endif
endfun

com! -nargs=? UpdateGhcTagsPlugin :call UpdateGhcTagsPlugin(<q-args>)
