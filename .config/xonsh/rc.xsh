# adjust some paths
$PATH.append('/home/rafael049/.local/bin')
$LD_LIBRARY_PATH = ['/home/rafael049/.local/lib', '']

# alias to quit AwesomeWM from the terminal
def _hello(args, stdin=None):
		print('hello')

aliases['qa'] = _hello
aliases['ls'] = 'exa -la --group-directories-first'
aliases['rm'] = 'rm -i'
aliases['mv'] = 'mv -i'
aliases['py'] = 'python3'

# some customization options, see https://xon.sh/envvars.html for details
$MULTILINE_PROMPT = '`·.,¸,.·*¯`·.,¸,.·*¯'
$XONSH_SHOW_TRACEBACK = True
$XONSH_STORE_STDOUT = True
$XONSH_HISTORY_MATCH_ANYWHERE = True
$COMPLETIONS_CONFIRM = True
$XONSH_AUTOPAIR = True
