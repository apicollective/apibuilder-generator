package lib

import javax.inject.Inject
import play.api.http.EnabledFilters
import play.filters.gzip.GzipFilter

class Filters @Inject() (
  defaultFilters: EnabledFilters,
  gzip: GzipFilter,
  log: LoggingFilter)
