def can_exec(path):
    import os
    return os.path.exists(path) and os.path.isfile(path) and os.access(path, os.X_OK)

def find_executable(name, default_path, extra_dirs = [], where = '.'):
    import os
    # print "Checking for %s" % name
    if can_exec(os.path.join(where, name)):
        return os.path.join(where, name)
    for dir in extra_dirs:
        # full = "%s/%s" % (dir,name)
        # print "Checking for %s" % full
        full = os.path.join(where, dir, name)
        if can_exec(full):
            return full
    next = '../' + where
    if os.path.abspath(next) == os.path.abspath(where):
        return os.path.expanduser(default_path)
    return find_executable(name, default_path, extra_dirs, next)

def run(path, *extra_args):
    import os
    import sys
    # all = [path] + list(extra_args) + sys.argv[1:]
    all = [path] + list(extra_args)
    print "Executing %s" % all
    # print sys.argv
    os.execve(path, all, os.environ)
