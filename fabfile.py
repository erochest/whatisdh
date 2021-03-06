

from fabric.api import cd, env, lcd, local, prefix, run, task


env.hosts = ['haskell-build.dev']
env.user = 'vagrant'
env.password = 'vagrant'


# Main Tasks

@task
def vagrant_up():
    with lcd('~/p/haskell-build/'):
        local('vagrant up')


@task
def init():
    with cd('~'):
        run('git clone ssh://err8n@host.dev/Users/err8n/p/whatisdh/')
    with cd('~/whatisdh'):
        run('hsenv')
        with prefix(hsact('whatisdh')):
            run('cabal update')
            run('cabal install cabal-src')
            run('cabal install yesod-platform')


@task
def build():
    with cd('~/whatisdh'):
        checkout_master()
        git_pull()
        git_checkout('deploy', True, True)

        # run('[ ! -s dist ] && ln -s dist_whatisdh dist')
        with prefix(hsact('whatisdh')):
            run('cabal clean')
            run('cabal install')

        strip('whatisdh')
        run('git add whatisdh')
        run('git commit -m deploy')
        checkout_master()


@task
def deploy():
    local('git fetch build deploy:deploy')
    local('git push --force heroku deploy:master')


@task
def clean_deploy():
    local('git branch -D deploy')
    local('git push build :deploy')


@task
def vagrant_suspend():
    with lcd('~/p/haskell-build/'):
        local('vagrant suspend')


@task
def destroy():
    run('rm -rf ~/whatisdh')


@task
def maintenance(state='on'):
    """Turns maintenance on and off on heroku.

    Takes a 'state' parameter with the value 'on' (default) or 'off'.

    """

    assert state in ('on', 'off')
    local('heroku maintenance:{0}'.format(state))


# Secondary Tasks


@task
def git_checkout(branch, create=False, force=False):
    b = '-b ' if create else ''

    # A work-around for not having -B is earlier versions of git. This is not
    # transactional, though.
    if b and force:
        b = ''
        run("git branch -f '%s'" % (branch,))

    run("git checkout %s '%s'" % (b, branch,))


@task
def checkout_master():
    git_checkout('master')


@task
def git_pull():
    run("git pull")


@task
def strip(project):
    run((
            'strip -p --strip-unneeded --remove-section=.comment '
            '-o ./{project} '
            'dist_{project}/build/{project}/{project}'
        ).format(project=project))


def hsact(project):
    return 'source .hsenv_{0}/bin/activate'.format(project)


# vim: set filetype=python:
