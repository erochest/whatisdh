

from fabric.api import cd, env, local, prefix, run, task


env.hosts = ['haskell-build.dev']
env.user = 'vagrant'
env.password = 'vagrant'


# Main Tasks

@task
def vagrant_up():
    local('pushd ~/p/haskell-build/ ; vagrant up ; popd')


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

        # run('[ ! -s dist ] && ln -s dist_whatisdh dist')
        with prefix(hsact('whatisdh')):
            run('cabal clean')
            run('cabal install')

        git_checkout('deploy', True)
        strip('whatisdh')
        run('git add whatisdh')
        run('git commit -m deploy')
        checkout_master()


@task
def get_deploy():
    local('git pull build')
    local('git push heroku deploy:master')


@task
def clean_deploy():
    local('git branch -D deploy')
    local('git push build :deploy')


@task
def vagrant_suspend():
    local('pushd ~/p/haskell-build/ ; vagrant suspend ; popd')


@task
def destroy():
    run('rm -rf ~/whatisdh')


# Secondary Tasks


@task
def git_checkout(branch, create=False):
    b = '-b ' if create else ''
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
